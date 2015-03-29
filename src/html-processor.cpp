// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "html-processor.hpp"

#include "fo-processor.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"
#include "estd/memory.hpp"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>
#include <boost/optional/optional.hpp>

#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>


namespace eyestep {

namespace fs = boost::filesystem;

const std::string k_SAIRY_GENERATOR = "Sairy HTML Processor";

detail::HtmlRenderContext::HtmlRenderContext() : _caps(detail::k_normal_caps)
{
}

html::Writer& detail::HtmlRenderContext::port()
{
  return *_port.get();
}

boost::filesystem::path detail::HtmlRenderContext::current_path()
{
  return _path;
}

void detail::HtmlRenderContext::push_port(std::unique_ptr<html::Writer> port,
                                          const fs::path& path)
{
  _ports.push_front(
    std::tuple<std::unique_ptr<html::Writer>, fs::path>(std::move(_port),
                                                        _path));
  _port = std::move(port);
  _path = path;
}

void detail::HtmlRenderContext::pop_port()
{
  if (!_ports.empty()) {
    auto tup = std::move(_ports.front());
    _port = std::move(std::get<0>(tup));
    _path = std::get<1>(tup);
    _ports.pop_front();
  }
}


detail::CapsStyle detail::HtmlRenderContext::capsstyle()
{
  return _caps;
}

void detail::HtmlRenderContext::set_capsstyle(CapsStyle capsstyle)
{
  _caps = capsstyle;
}


void detail::HtmlRenderContext::push_cssmap(const CssAttrMap& map)
{
  _css_stack.push_front(map);
}


void detail::HtmlRenderContext::pop_cssmap()
{
  if (!_css_stack.empty()) {
    _css_stack.pop_front();
  }
}

boost::optional<std::string>
detail::HtmlRenderContext::css_property(const std::string& key) const
{
  for (const auto& map : _css_stack) {
    const auto i_find = map.find(key);
    if (i_find != map.end()) {
      return i_find->second;
    }
  }

  return boost::none;
}


//------------------------------------------------------------------------------

namespace {
  std::string dimen2css(const fo::Dimen& dim)
  {
    auto unit_name = [](fo::Unit un) {
      switch (un) {
      case fo::k_pt:
        return "pt";
      case fo::k_m:
        return "m";
      case fo::k_mm:
        return "mm";
      case fo::k_cm:
        return "cm";
      case fo::k_em:
        return "em";
      }
    };

    std::stringstream ss;
    ss << dim._value << unit_name(dim._unit);
    return ss.str();
  }


  detail::CssAttrMap new_css_attrs(const detail::HtmlRenderContext& ctx,
                                   const detail::CssAttrMap& attrs)
  {
    detail::CssAttrMap result;

    for (const auto& pair : attrs) {
      auto val = ctx.css_property(pair.first);
      if (!val || *val != pair.second) {
        result[pair.first] = pair.second;
      }
    }

    return result;
  }


  std::string css_attr_to_string(const detail::CssAttrMap& map)
  {
    std::stringstream ss;
    for (const auto& pair : map) {
      ss << pair.first << ": " << pair.second << "; ";
    }
    return ss.str();
  }


  template <typename T>
  void set_css_attr(detail::CssAttrMap& map, const std::string& key,
                    boost::optional<T> val_or_none)
  {
    if (val_or_none) {
      map[key] = *val_or_none;
    }
  }


  void set_css_attr(detail::CssAttrMap& map, const std::string& key,
                    boost::optional<fo::Dimen> val_or_none)
  {
    if (val_or_none) {
      map[key] = dimen2css(*val_or_none);
    }
  }


  void set_css_attr(detail::CssAttrMap& map, const std::string& key,
                    const std::string& val)
  {
    map[key] = val;
  }


  void set_css_capsstyle(detail::CssAttrMap& map, HtmlProcessor* processor,
                         boost::optional<std::string> val_or_none)
  {
    if (val_or_none) {
      if (*val_or_none == std::string("normal")) {
        processor->ctx().set_capsstyle(detail::k_normal_caps);
      }
      else if (*val_or_none == std::string("caps")) {
        processor->ctx().set_capsstyle(detail::k_upper_caps);
      }
      else if (*val_or_none == std::string("small-caps")) {
        processor->ctx().set_capsstyle(detail::k_small_caps);
        map["font-variant"] = "small-caps";
      }
    }
  }


  html::Tag css_optional_tag(HtmlProcessor* processor, const std::string& tag,
                             boost::optional<std::string> val_or_none,
                             const std::string& exp_val)
  {
    if (val_or_none) {
      if (*val_or_none == exp_val) {
        return std::move(html::Tag(processor->ctx().port(), tag));
      }
    }

    return std::move(html::Tag());
  }


  html::Attrs style_str2attrs(const std::string& str)
  {
    html::Attrs attrs;
    if (!str.empty()) {
      attrs.push_back(html::Attr{"style", str});
    }

    return attrs;
  }


  html::Tag css_optional_span_tag(HtmlProcessor* processor,
                                  const detail::CssAttrMap& attrs)
  {
    if (!attrs.empty()) {
      return std::move(html::Tag(processor->ctx().port(), "span",
                                 style_str2attrs(css_attr_to_string(attrs))));
    }

    return std::move(html::Tag());
  }


  void set_css_font_characteristics(detail::CssAttrMap& map,
                                    HtmlProcessor* processor,
                                    const IFormattingObject* fo)
  {
    auto fontsize = processor->property_or_none<fo::Dimen>(fo, "font-size");
    // auto fontWeight =
    //   processor->property_or_none<std::string>(fo, "font-weight");
    // auto fontPosture =
    //   processor->property_or_none<std::string>(fo, "font-posture");
    auto fontcaps = processor->property_or_none<std::string>(fo, "font-caps");
    auto posptshift =
      processor->property_or_none<fo::Dimen>(fo, "position-point-shift");

    set_css_attr(map, "font-size", fontsize);
    set_css_capsstyle(map, processor, fontcaps);
    set_css_attr(map, "position", "relative");
    set_css_attr(map, "top", posptshift);
  }


  void set_css_space_characteristics(detail::CssAttrMap& map,
                                     HtmlProcessor* processor,
                                     const IFormattingObject* fo)
  {
    auto startindent =
      processor->property_or_none<fo::Dimen>(fo, "start-indent");
    auto endindent = processor->property_or_none<fo::Dimen>(fo, "end-indent");
    auto firstline_startindent =
      processor->property_or_none<fo::Dimen>(fo, "first-line-start-indent");
    auto spacebefore =
      processor->property_or_none<fo::Dimen>(fo, "space-before");
    auto spaceafter = processor->property_or_none<fo::Dimen>(fo, "space-after");

    set_css_attr(map, "margin-left", startindent);
    set_css_attr(map, "margin-right", endindent);
    set_css_attr(map, "margin-top", spacebefore);
    set_css_attr(map, "margin-bottom", spaceafter);
    set_css_attr(map, "text-indent", firstline_startindent);
  }


  template <typename Functor>
  void withHtmlFile(HtmlProcessor* processor, const fs::path& path,
                    const std::string& title, const std::string& author,
                    const std::string& desc, const html::Doctype& doctype,
                    Functor functor)
  {
    detail::HtmlRenderContext& ctx = processor->ctx();

    auto port = estd::make_unique<html::Writer>(doctype, k_SAIRY_GENERATOR);
    port->open(path);

    ctx.push_port(std::move(port), path);

    ctx.port().header(title, author, desc, [](std::ostream&) {});

    functor(processor);

    ctx.port().footer();
    ctx.pop_port();
  }


  class HtmlLiteralFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      auto str = static_cast<const fo::Literal*>(fo)->text();

      auto capsstyle = processor->ctx().capsstyle();
      if (capsstyle == detail::k_normal_caps) {
        processor->ctx().port().write_text(str);
      }
      else if (capsstyle == detail::k_upper_caps) {
        processor->ctx().port().write_text(boost::to_upper_copy(str));
      }
      else if (capsstyle == detail::k_small_caps) {
        // html::Tag with_Tag(processor->ctx().port(), "span",
        //                    {html::Attr{"style", "font-variant:
        //                    small-caps;"}});
        processor->ctx().port().write_text(str);
      }
    }
  };


  class HtmlParagraphFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      detail::CssAttrMap map;
      set_css_font_characteristics(map, processor, fo);
      set_css_space_characteristics(map, processor, fo);

      {
        auto css_attrs = new_css_attrs(processor->ctx(), map);
        html::Tag with_tag(processor->ctx().port(), "p",
                           style_str2attrs(css_attr_to_string(css_attrs)));
        processor->ctx().push_cssmap(css_attrs);

        html::Tag b_tag(std::move(
          css_optional_tag(processor, "b",
                           processor
                             ->property_or_none<std::string>(fo, "font-weight"),
                           "bold")));
        html::Tag i_tag(std::move(
          css_optional_tag(processor, "i", processor->property_or_none<
                                             std::string>(fo, "font-posture"),
                           "italic")));

        processor->render_sosofo(&fo->port("text"));

        processor->ctx().pop_cssmap();
      }
      processor->ctx().port().newln();
    }
  };


  class HtmlParagraphBreakFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      processor->ctx().port().empty_tag("br");
      processor->ctx().port().newln();
    }
  };


  class HtmlDisplayGroupFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      detail::CssAttrMap map;
      set_css_font_characteristics(map, processor, fo);
      set_css_space_characteristics(map, processor, fo);

      {
        auto css_attrs = new_css_attrs(processor->ctx(), map);
        html::Tag with_tag(processor->ctx().port(), "div",
                           style_str2attrs(css_attr_to_string(css_attrs)));
        processor->ctx().push_cssmap(css_attrs);

        processor->ctx().port().newln();
        processor->render_sosofo(&fo->port("text"));

        processor->ctx().pop_cssmap();
      }
      processor->ctx().port().newln();
    }
  };


  class HtmlSimplePageSequenceFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      auto title = processor->property(fo, "metadata:title", std::string());
      auto author = processor->property(fo, "metadata:author", std::string());
      auto desc = processor->property(fo, "metadata:desc", std::string());

      withHtmlFile(processor, processor->ctx().current_path(), title, author,
                   desc, html::k_XHTML_1_1_DTD, [fo](HtmlProcessor* processor) {
                     {
                       html::Tag with_tag(processor->ctx().port(), "div",
                                          {html::Attr{"class", "page"}});
                       processor->ctx().port().newln();

                       processor->render_sosofo(&fo->port("text"));
                     }
                     processor->ctx().port().newln();
                   });
    }
  };


  class HtmlSequenceFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      detail::CssAttrMap map;
      set_css_font_characteristics(map, processor, fo);

      {
        auto css_attrs = new_css_attrs(processor->ctx(), map);
        html::Tag span_tag(css_optional_span_tag(processor, css_attrs));
        processor->ctx().push_cssmap(css_attrs);

        html::Tag b_tag(std::move(
          css_optional_tag(processor, "b",
                           processor
                             ->property_or_none<std::string>(fo, "font-weight"),
                           "bold")));
        html::Tag i_tag(std::move(
          css_optional_tag(processor, "i", processor->property_or_none<
                                             std::string>(fo, "font-posture"),
                           "italic")));

        processor->render_sosofo(&fo->port("text"));

        processor->ctx().pop_cssmap();
      }
    }
  };

} // ns anon


HtmlProcessor::HtmlProcessor()
{
}


std::string HtmlProcessor::proc_id() const
{
  return "#html-processor";
}


std::string HtmlProcessor::default_output_extension() const
{
  return ".html";
}

const IFoProcessor<HtmlProcessor>*
HtmlProcessor::lookup_fo_processor(const std::string& fo_classname) const
{
  static auto procs =
    std::map<std::string, std::shared_ptr<IFoProcessor<HtmlProcessor>>>{
      {"#literal", std::make_shared<HtmlLiteralFoProcessor>()},
      {"#paragraph", std::make_shared<HtmlParagraphFoProcessor>()},
      {"#paragraph-break", std::make_shared<HtmlParagraphBreakFoProcessor>()},
      {"#display-group", std::make_shared<HtmlDisplayGroupFoProcessor>()},
      {"#simple-page-sequence",
       std::make_shared<HtmlSimplePageSequenceFoProcessor>()},
      {"#sequence", std::make_shared<HtmlSequenceFoProcessor>()},
    };

  auto i_find = procs.find(fo_classname);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void HtmlProcessor::before_rendering()
{
  auto mainport =
    estd::make_unique<html::Writer>(html::k_XHTML_1_1_DTD, k_SAIRY_GENERATOR);
  _ctx.push_port(std::move(mainport), _output_file);
}


void HtmlProcessor::after_rendering()
{
}


detail::HtmlRenderContext& HtmlProcessor::ctx()
{
  return _ctx;
}


html::Writer& HtmlProcessor::writer()
{
  return _ctx.port();
}


} // ns eyestep
