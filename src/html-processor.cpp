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

const std::string kSairyGenerator = "Sairy HTML Processor";

detail::HtmlRenderContext::HtmlRenderContext() : mCaps(detail::kNormalCaps)
{
}

html::Writer& detail::HtmlRenderContext::port()
{
  return *mPort.get();
}

boost::filesystem::path detail::HtmlRenderContext::currentPath()
{
  return mPath;
}

void detail::HtmlRenderContext::pushPort(std::unique_ptr<html::Writer> port,
                                         const fs::path& path)
{
  mPorts.push_front(
    std::tuple<std::unique_ptr<html::Writer>, fs::path>(std::move(mPort),
                                                        mPath));
  mPort = std::move(port);
  mPath = path;
}

void detail::HtmlRenderContext::popPort()
{
  if (!mPorts.empty()) {
    auto tup = std::move(mPorts.front());
    mPort = std::move(std::get<0>(tup));
    mPath = std::get<1>(tup);
    mPorts.pop_front();
  }
}


detail::CapsStyle detail::HtmlRenderContext::capsStyle()
{
  return mCaps;
}

void detail::HtmlRenderContext::setCapsStyle(CapsStyle capsStyle)
{
  mCaps = capsStyle;
}


void detail::HtmlRenderContext::pushCssMap(const CssAttrMap& map)
{
  mCssStack.push_front(map);
}


void detail::HtmlRenderContext::popCssMap()
{
  if (!mCssStack.empty()) {
    mCssStack.pop_front();
  }
}

boost::optional<std::string>
detail::HtmlRenderContext::cssProperty(const std::string& key) const
{
  for (const auto& map : mCssStack) {
    const auto i_find = map.find(key);
    if (i_find != map.end()) {
      return i_find->second;
    }
  }

  return boost::none;
}


//------------------------------------------------------------------------------

namespace {
  std::string dimenToCss(const fo::Dimen& dim)
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
    ss << dim.mValue << unit_name(dim.mUnit);
    return ss.str();
  }


  detail::CssAttrMap new_css_attrs(const detail::HtmlRenderContext& ctx,
                                   const detail::CssAttrMap& attrs)
  {
    detail::CssAttrMap result;

    for (const auto& pair : attrs) {
      auto val = ctx.cssProperty(pair.first);
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
      map[key] = dimenToCss(*val_or_none);
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
        processor->ctx().setCapsStyle(detail::kNormalCaps);
      }
      else if (*val_or_none == std::string("caps")) {
        processor->ctx().setCapsStyle(detail::kUpperCaps);
      }
      else if (*val_or_none == std::string("small-caps")) {
        processor->ctx().setCapsStyle(detail::kSmallCaps);
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


  html::Attrs styleStrToAttrs(const std::string& str)
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
                                 styleStrToAttrs(css_attr_to_string(attrs))));
    }

    return std::move(html::Tag());
  }


  void set_css_font_characteristics(detail::CssAttrMap& map,
                                    HtmlProcessor* processor,
                                    const IFormattingObject* fo)
  {
    auto fontSize = processor->propertyOrNone<fo::Dimen>(fo, "font-size");
    // auto fontWeight =
    //   processor->propertyOrNone<std::string>(fo, "font-weight");
    // auto fontPosture =
    //   processor->propertyOrNone<std::string>(fo, "font-posture");
    auto fontCaps = processor->propertyOrNone<std::string>(fo, "font-caps");
    auto posPtShift =
      processor->propertyOrNone<fo::Dimen>(fo, "position-point-shift");

    set_css_attr(map, "font-size", fontSize);
    set_css_capsstyle(map, processor, fontCaps);
    set_css_attr(map, "position", "relative");
    set_css_attr(map, "top", posPtShift);
  }


  void set_css_space_characteristics(detail::CssAttrMap& map,
                                     HtmlProcessor* processor,
                                     const IFormattingObject* fo)
  {
    auto startIndent = processor->propertyOrNone<fo::Dimen>(fo, "start-indent");
    auto endIndent = processor->propertyOrNone<fo::Dimen>(fo, "end-indent");
    auto firstLineStartIndent =
      processor->propertyOrNone<fo::Dimen>(fo, "first-line-start-indent");
    auto spaceBefore = processor->propertyOrNone<fo::Dimen>(fo, "space-before");
    auto spaceAfter = processor->propertyOrNone<fo::Dimen>(fo, "space-after");

    set_css_attr(map, "margin-left", startIndent);
    set_css_attr(map, "margin-right", endIndent);
    set_css_attr(map, "margin-top", spaceBefore);
    set_css_attr(map, "margin-bottom", spaceAfter);
    set_css_attr(map, "first-line-start-indent", firstLineStartIndent);
  }


  template <typename Functor>
  void withHtmlFile(HtmlProcessor* processor, const fs::path& path,
                    const std::string& title, const std::string& author,
                    const std::string& desc, const html::Doctype& doctype,
                    Functor functor)
  {
    detail::HtmlRenderContext& ctx = processor->ctx();

    auto port = estd::make_unique<html::Writer>(doctype, kSairyGenerator);
    port->open(path);

    ctx.pushPort(std::move(port), path);

    ctx.port().header(title, author, desc, [](std::ostream&) {});

    functor(processor);

    ctx.port().footer();
    ctx.popPort();
  }


  class HtmlLiteralFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      auto str = static_cast<const fo::Literal*>(fo)->text();

      auto capsStyle = processor->ctx().capsStyle();
      if (capsStyle == detail::kNormalCaps) {
        processor->ctx().port().write_text(str);
      }
      else if (capsStyle == detail::kUpperCaps) {
        processor->ctx().port().write_text(boost::to_upper_copy(str));
      }
      else if (capsStyle == detail::kSmallCaps) {
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
                           styleStrToAttrs(css_attr_to_string(css_attrs)));
        processor->ctx().pushCssMap(css_attrs);

        html::Tag b_tag(std::move(
          css_optional_tag(processor, "b",
                           processor
                             ->propertyOrNone<std::string>(fo, "font-weight"),
                           "bold")));
        html::Tag i_tag(std::move(
          css_optional_tag(processor, "i",
                           processor
                             ->propertyOrNone<std::string>(fo, "font-posture"),
                           "italic")));

        processor->renderSosofo(&fo->port("text"));

        processor->ctx().popCssMap();
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
                           styleStrToAttrs(css_attr_to_string(css_attrs)));
        processor->ctx().pushCssMap(css_attrs);

        processor->ctx().port().newln();
        processor->renderSosofo(&fo->port("text"));

        processor->ctx().popCssMap();
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

      withHtmlFile(processor, processor->ctx().currentPath(), title, author,
                   desc, html::kXHTML_1_1_DTD, [fo](HtmlProcessor* processor) {
                     {
                       html::Tag with_tag(processor->ctx().port(), "div",
                                          {html::Attr{"class", "page"}});
                       processor->ctx().port().newln();

                       processor->renderSosofo(&fo->port("text"));
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
        processor->ctx().pushCssMap(css_attrs);

        html::Tag b_tag(std::move(
          css_optional_tag(processor, "b",
                           processor
                             ->propertyOrNone<std::string>(fo, "font-weight"),
                           "bold")));
        html::Tag i_tag(std::move(
          css_optional_tag(processor, "i",
                           processor
                             ->propertyOrNone<std::string>(fo, "font-posture"),
                           "italic")));

        processor->renderSosofo(&fo->port("text"));

        processor->ctx().popCssMap();
      }
    }
  };

} // ns anon


HtmlProcessor::HtmlProcessor()
{
}


std::string HtmlProcessor::procId() const
{
  return "#html-processor";
}


const IFoProcessor<HtmlProcessor>*
HtmlProcessor::lookupFoProcessor(const std::string& foClassName) const
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

  auto i_find = procs.find(foClassName);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void HtmlProcessor::beforeRendering()
{
  auto mainPort =
    estd::make_unique<html::Writer>(html::kXHTML_1_1_DTD, kSairyGenerator);
  mCtx.pushPort(std::move(mainPort), mOutputFile);
}


void HtmlProcessor::afterRendering()
{
}


detail::HtmlRenderContext& HtmlProcessor::ctx()
{
  return mCtx;
}


html::Writer& HtmlProcessor::writer()
{
  return mCtx.port();
}


} // ns eyestep
