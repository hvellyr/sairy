// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "html-processor.hpp"

#include "fo-processor.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"
#include "estd/memory.hpp"

#include "program_options/program_options.hpp"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>
#include <boost/optional/optional.hpp>

#include <iostream>
#include <map>
#include <memory>
#include <unordered_set>
#include <sstream>
#include <string>


namespace eyestep {

namespace fs = boost::filesystem;
namespace po = program_options;

const std::string k_TEXTBOOK_GENERATOR = "Textbook HTML Processor";


detail::HtmlRenderContext::HtmlRenderContext()
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
  _ports.push_front(std::make_tuple(std::move(_port), _path));
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
  if (!_styles_stack.empty()) {
    return _styles_stack.front()._caps;
  }

  return k_normal_caps;
}

void detail::HtmlRenderContext::push_styles(const StyleAttrs& styles)
{
  _styles_stack.push_front(styles);
}


void detail::HtmlRenderContext::pop_styles()
{
  if (!_styles_stack.empty()) {
    _styles_stack.pop_front();
  }
}

boost::optional<std::string>
detail::HtmlRenderContext::css_property(const std::string& key) const
{
  for (const auto& styles : _styles_stack) {
    const auto i_find = styles._css_map.find(key);
    if (i_find != styles._css_map.end()) {
      return i_find->second;
    }
  }

  return boost::none;
}


//------------------------------------------------------------------------------

namespace {
  const std::string k_pt = "pt";
  const std::string k_px = "px";
  const std::string k_em = "em";
  const std::string k_m = "m";
  const std::string k_mm = "mm";
  const std::string k_cm = "cm";
  const std::string k_in = "in";

  std::string length_spec2css(const fo::LengthSpec& dim)
  {
    auto unit_name = [](fo::Unit un) {
      switch (un) {
      case fo::k_pt:
        return k_pt;
      case fo::k_px:
        return k_px;
      case fo::k_m:
        return k_m;
      case fo::k_mm:
        return k_mm;
      case fo::k_cm:
        return k_cm;
      case fo::k_em:
        return k_em;
      case fo::k_in:
        return k_in;
      }
    };

    std::stringstream ss;
    ss << dim._value << unit_name(dim._unit);
    return ss.str();
  }


  bool css_attribute_is_inherited(const std::string& prop)
  {
    static const std::unordered_set<std::string> not_inherited =
      {"padding-left", "padding-bottom", "padding-right", "padding-top",
       "padding", "height", "width", "bottom", "left", "right", "top",
       "display", "margin", "margin-left", "margin-right", "margin-bottom",
       "margin-top"};
    return not_inherited.find(prop) == not_inherited.end();
  }


  detail::StyleAttrs intersect_css_attrs(const detail::HtmlRenderContext& ctx,
                                         const detail::StyleAttrs& attrs)
  {
    detail::CssAttrMap result;

    for (const auto& pair : attrs._css_map) {
      auto val = ctx.css_property(pair.first);
      if (!val || *val != pair.second ||
          !css_attribute_is_inherited(pair.first)) {
        result[pair.first] = pair.second;
      }
    }

    return detail::StyleAttrs{attrs._caps, result};
  }


  std::string attrs_to_string(const detail::StyleAttrs& attrs)
  {
    std::stringstream ss;
    for (const auto& pair : attrs._css_map) {
      if (!pair.first.empty()) {
        ss << pair.first << ": " << pair.second << "; ";
      }
    }
    return ss.str();
  }


  template <typename T>
  void set_attr(detail::StyleAttrs& attrs, const std::string& key,
                boost::optional<T> val_or_none)
  {
    if (val_or_none) {
      attrs._css_map[key] = *val_or_none;
    }
  }


  void set_attr(detail::StyleAttrs& attrs, const std::string& key,
                boost::optional<fo::LengthSpec> val_or_none)
  {
    if (val_or_none) {
      attrs._css_map[key] = length_spec2css(*val_or_none);
    }
  }


  void set_attr(detail::StyleAttrs& attrs, const std::string& key,
                const std::string& val)
  {
    attrs._css_map[key] = val;
  }


  const std::string k_normal = "normal";
  const std::string k_lower = "lower";
  const std::string k_caps = "caps";
  const std::string k_small_caps = "small-caps";

  void set_capsstyle(detail::StyleAttrs& attrs,
                     boost::optional<std::string> val_or_none)
  {
    if (val_or_none) {
      if (*val_or_none == k_normal) {
        attrs._caps = detail::k_normal_caps;
      }
      else if (*val_or_none == k_lower) {
        attrs._caps = detail::k_lower_caps;
      }
      else if (*val_or_none == k_caps) {
        attrs._caps = detail::k_upper_caps;
      }
      else if (*val_or_none == k_small_caps) {
        attrs._caps = detail::k_small_caps;
        attrs._css_map["font-variant"] = k_small_caps;
      }
    }
  }


  html::Tag optional_tag(HtmlProcessor* processor, const std::string& tag,
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


  html::Tag optional_span_tag(HtmlProcessor* processor,
                              const detail::StyleAttrs& attrs)
  {
    if (!attrs._css_map.empty()) {
      return std::move(html::Tag(processor->ctx().port(), "span",
                                 style_str2attrs(attrs_to_string(attrs))));
    }

    return std::move(html::Tag());
  }

  html::Tag optional_inline_block_tag(HtmlProcessor* processor,
                                      const detail::StyleAttrs& attrs)
  {
    detail::StyleAttrs extAttrs(attrs);
    extAttrs._css_map["display"] = "inline-block";
    extAttrs._css_map["text-indent"] = "0em";

    if (!extAttrs._css_map.empty()) {
      return std::move(html::Tag(processor->ctx().port(), "span",
                                 style_str2attrs(attrs_to_string(extAttrs))));
    }

    return std::move(html::Tag());
  }


  void set_font_characteristics(detail::StyleAttrs& attrs,
                                HtmlProcessor* processor,
                                const IFormattingObject* fo)
  {
    auto fontsize = processor->property_or_none<fo::LengthSpec>(fo, "font-size");
    auto fontcaps = processor->property_or_none<std::string>(fo, "font-caps");
    auto posptshift =
      processor->property_or_none<fo::LengthSpec>(fo, "position-point-shift");

    set_attr(attrs, "font-size", fontsize);
    set_capsstyle(attrs, fontcaps);
    set_attr(attrs, "position", "relative");
    set_attr(attrs, "top", posptshift);
  }


  void set_space_characteristics(detail::StyleAttrs& attrs,
                                 HtmlProcessor* processor,
                                 const IFormattingObject* fo)
  {
    auto startindent =
      processor->property_or_none<fo::LengthSpec>(fo, "start-indent");
    auto endindent = processor->property_or_none<fo::LengthSpec>(fo, "end-indent");
    auto firstline_startindent =
      processor->property_or_none<fo::LengthSpec>(fo, "first-line-start-indent");
    auto spacebefore =
      processor->property_or_none<fo::LengthSpec>(fo, "space-before");
    auto spaceafter = processor->property_or_none<fo::LengthSpec>(fo, "space-after");

    set_attr(attrs, "padding-left", startindent);
    set_attr(attrs, "margin-right", endindent);
    set_attr(attrs, "margin-top", spacebefore);
    set_attr(attrs, "margin-bottom", spaceafter);
    set_attr(attrs, "text-indent", firstline_startindent);
  }


  class StyleScope {
    detail::HtmlRenderContext& _ctx;

  public:
    StyleScope(detail::HtmlRenderContext& ctx, const detail::StyleAttrs& attrs)
      : _ctx(ctx)
    {
      _ctx.push_styles(attrs);
    }

    ~StyleScope() { _ctx.pop_styles(); }
  };


  template <typename Functor>
  void withHtmlFile(HtmlProcessor* processor, const fs::path& path,
                    const std::string& title, const std::string& author,
                    const std::string& desc, const html::Doctype& doctype,
                    Functor functor)
  {
    if (processor->is_verbose()) {
      std::cout << processor->proc_id() << ": Create output file: '"
                << path.string() << "'" << std::endl;
    }

    detail::HtmlRenderContext& ctx = processor->ctx();

    auto port = estd::make_unique<html::Writer>(doctype, k_TEXTBOOK_GENERATOR);
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
      else if (capsstyle == detail::k_lower_caps) {
        processor->ctx().port().write_text(boost::to_lower_copy(str));
      }
      else if (capsstyle == detail::k_upper_caps) {
        processor->ctx().port().write_text(boost::to_upper_copy(str));
      }
      else if (capsstyle == detail::k_small_caps) {
        processor->ctx().port().write_text(str);
      }
    }
  };


  class HtmlParagraphFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      detail::StyleAttrs attrs;
      set_font_characteristics(attrs, processor, fo);
      set_space_characteristics(attrs, processor, fo);

      {
        auto d_attrs = intersect_css_attrs(processor->ctx(), attrs);
        html::Tag with_tag(processor->ctx().port(), "p",
                           style_str2attrs(attrs_to_string(d_attrs)));
        StyleScope style_scope(processor->ctx(), d_attrs);

        html::Tag b_tag(std::move(
          optional_tag(processor, "b",
                       processor->property_or_none<std::string>(fo,
                                                                "font-weight"),
                       "bold")));
        html::Tag i_tag(std::move(
          optional_tag(processor, "i",
                       processor->property_or_none<std::string>(fo,
                                                                "font-posture"),
                       "italic")));

        processor->render_sosofo(&fo->port("text"));
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
      detail::StyleAttrs attrs;
      set_font_characteristics(attrs, processor, fo);
      set_space_characteristics(attrs, processor, fo);

      {
        auto d_attrs = intersect_css_attrs(processor->ctx(), attrs);
        html::Tag with_tag(processor->ctx().port(), "div",
                           style_str2attrs(attrs_to_string(d_attrs)));
        StyleScope style_scope(processor->ctx(), d_attrs);

        processor->ctx().port().newln();
        processor->render_sosofo(&fo->port("text"));
      }
      processor->ctx().port().newln();
    }
  };


  class HtmlSimplePageSequenceFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      auto title = processor->property(fo, "metadata.title", std::string());
      auto author = processor->property(fo, "metadata.author", std::string());
      auto desc = processor->property(fo, "metadata.desc", std::string());
      auto html_width =
        processor->property_or_none<fo::LengthSpec>(fo, "html.width");

      withHtmlFile(processor, processor->ctx().current_path(), title, author,
                   desc, html::k_XHTML_1_1_DTD,
                   [fo, html_width](HtmlProcessor* processor) {
                     {
                       detail::StyleAttrs attrs;
                       set_attr(attrs, "width", html_width);

                       html::Tag with_tag(processor->ctx().port(), "div",
                                          style_str2attrs(
                                            attrs_to_string(attrs)));
                       StyleScope style_scope(processor->ctx(), attrs);

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
      detail::StyleAttrs attrs;
      set_font_characteristics(attrs, processor, fo);

      {
        auto d_attrs = intersect_css_attrs(processor->ctx(), attrs);
        html::Tag span_tag(optional_span_tag(processor, d_attrs));
        StyleScope style_scope(processor->ctx(), d_attrs);

        html::Tag b_tag(std::move(
          optional_tag(processor, "b",
                       processor->property_or_none<std::string>(fo,
                                                                "font-weight"),
                       "bold")));
        html::Tag i_tag(std::move(
          optional_tag(processor, "i",
                       processor->property_or_none<std::string>(fo,
                                                                "font-posture"),
                       "italic")));

        processor->render_sosofo(&fo->port("text"));
      }
    }
  };


  const std::string k_left = "left";
  const std::string k_center = "center";
  const std::string k_right = "right";

  class HtmlLineFieldFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      detail::StyleAttrs attrs;
      set_font_characteristics(attrs, processor, fo);

      auto field_width =
        processor->property_or_none<fo::LengthSpec>(fo, "field-width");
      auto field_align =
        processor->property_or_none<std::string>(fo, "field-align");

      set_attr(attrs, "width", field_width);
      if (field_align) {
        if (*field_align == k_left || *field_align == k_center ||
            *field_align == k_right) {
          set_attr(attrs, "text-align", *field_align);
        }
      }

      {
        auto d_attrs = intersect_css_attrs(processor->ctx(), attrs);

        html::Tag span_tag(field_width
                             ? optional_inline_block_tag(processor, d_attrs)
                             : optional_span_tag(processor, d_attrs));
        StyleScope style_scope(processor->ctx(), d_attrs);

        html::Tag b_tag(std::move(
          optional_tag(processor, "b",
                       processor->property_or_none<std::string>(fo,
                                                                "font-weight"),
                       "bold")));
        html::Tag i_tag(std::move(
          optional_tag(processor, "i",
                       processor->property_or_none<std::string>(fo,
                                                                "font-posture"),
                       "italic")));

        processor->render_sosofo(&fo->port("text"));
      }
    }
  };

} // ns anon


HtmlProcessor::HtmlProcessor() : _verbose(false)
{
}


HtmlProcessor::HtmlProcessor(const po::variables_map& args) : _verbose(false)
{
  if (!args.empty()) {
    _verbose = args.count("verbose") != 0;
  }
}


std::string HtmlProcessor::proc_id() const
{
  return "html";
}


std::string HtmlProcessor::default_output_extension() const
{
  return ".html";
}

po::options_description HtmlProcessor::program_options() const
{
  std::string opts_title =
    std::string("HTML renderer [selector: '") + proc_id() + "']";
  po::options_description desc(opts_title);

  return desc;
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
      {"#line-field", std::make_shared<HtmlLineFieldFoProcessor>()},
    };

  auto i_find = procs.find(fo_classname);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void HtmlProcessor::before_rendering()
{
  auto mainport =
    estd::make_unique<html::Writer>(html::k_XHTML_1_1_DTD, k_TEXTBOOK_GENERATOR);
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


bool HtmlProcessor::is_verbose() const
{
  return _verbose;
}

} // ns eyestep
