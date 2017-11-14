// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "html-processor.hpp"

#include "config.hpp"
#include "estd/memory.hpp"
#include "fo-processor.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"
#include "utils.hpp"

#include "program_options/program_options.hpp"

#include "fspp/estd/optional.hpp"
#include "fspp/filesystem.hpp"

#include <algorithm>
#include <cmath>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <tuple>
#include <unordered_set>


namespace eyestep {

namespace fs = filesystem;
namespace po = program_options;

const auto k_text = std::string("text");

const auto k_TEXTBOOK_GENERATOR =
  std::string{"Textbook HTML Processor vr. "} + TEXTBOOK_VERSION;


html::Writer& detail::HtmlRenderContext::port() {
  return *_port.get();
}


fs::path detail::HtmlRenderContext::current_path() {
  return _path;
}


void detail::HtmlRenderContext::push_port(std::unique_ptr<html::Writer> port,
                                          const fs::path& path) {
  _ports.push_front(std::make_tuple(std::move(_port), _path));
  _port = std::move(port);
  _path = path;
}


void detail::HtmlRenderContext::pop_port() {
  if (!_ports.empty()) {
    auto tup = std::move(_ports.front());
    _port = std::move(std::get<0>(tup));
    _path = std::get<1>(tup);
    _ports.pop_front();
  }
}


detail::CapsStyle detail::HtmlRenderContext::capsstyle() {
  if (!_styles_stack.empty())
    return _styles_stack.front()._caps;

  return k_normal_caps;
}


void detail::HtmlRenderContext::push_styles(const StyleAttrs& styles) {
  _styles_stack.push_front(styles);
}


void detail::HtmlRenderContext::pop_styles() {
  if (!_styles_stack.empty())
    _styles_stack.pop_front();
}

estd::optional<std::string>
detail::HtmlRenderContext::css_property(const std::string& key) const {
  for (const auto& styles : _styles_stack) {
    const auto i_find = styles._css_map.find(key);
    if (i_find != styles._css_map.end())
      return i_find->second;
  }

  return {};
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


  std::string length_spec2css(const fo::LengthSpec& dim) {
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


  bool css_attribute_is_inherited(const std::string& prop) {
    using namespace std;

    static const auto not_inherited = unordered_set<string>{
      "padding-left",  "padding-bottom",
      "padding-right", "padding-top",
      "padding",       "height",
      "width",         "bottom",
      "left",          "right",
      "top",           "display",
      "margin",        "margin-left",
      "margin-right",  "margin-bottom",
      "margin-top",    "background-color",
    };
    return not_inherited.find(prop) == end(not_inherited);
  }


  float normalize_co_value(float v) {
    if (v <= 0)
      v = 0.0f;
    if (v > 100.0f)
      v = 100.0f;
    return v;
  }


  std::tuple<int, int, int> convert_cmyk_to_rgb(fo::Color co) {
    auto c = normalize_co_value(co._cmyk._cyan) / 100;
    auto m = normalize_co_value(co._cmyk._magenta) / 100;
    auto y = normalize_co_value(co._cmyk._yellow) / 100;
    auto k = normalize_co_value(co._cmyk._black) / 100;

    auto r = 1 - std::min<float>(1, c * (1 - k) + k);
    auto g = 1 - std::min<float>(1, m * (1 - k) + k);
    auto b = 1 - std::min<float>(1, y * (1 - k) + k);

    return std::make_tuple(floor(r * 255), floor(g * 255), floor(b * 255));
  }


  std::tuple<int, int, int> convert_gray_to_rgb(fo::Color co) {
    auto result = normalize_co_value(co._gray);
    return std::make_tuple(floor(result * 255), floor(result * 255), floor(result * 255));
  }


  std::string enc_color(const fo::Color co) {
    const auto rco = [&]() -> std::tuple<int, int, int> {
      switch (co._space) {
      case fo::kRGB:
        return std::make_tuple(floor(co._rgb._red * 255), floor(co._rgb._green * 255),
                               floor(co._rgb._blue * 255));
      case fo::kCMYK:
        return convert_cmyk_to_rgb(co);
      case fo::kGray:
        return convert_gray_to_rgb(co);
      }
    }();

    std::stringstream ss;
    ss << "rgb(" << std::get<0>(rco) << ", " << std::get<1>(rco) << ", "
       << std::get<2>(rco) << ")";
    return ss.str();
  }


  detail::StyleAttrs intersect_css_attrs(const detail::HtmlRenderContext& ctx,
                                         const detail::StyleAttrs& attrs) {
    auto result = detail::CssAttrMap{};

    for (const auto& pair : attrs._css_map) {
      auto val = ctx.css_property(pair.first);
      if (!val || *val != pair.second || !css_attribute_is_inherited(pair.first)) {
        result[pair.first] = pair.second;
      }
    }

    return detail::StyleAttrs{attrs._caps, result};
  }


  std::string attrs_to_string(const detail::StyleAttrs& attrs) {
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
                estd::optional<T> val_or_none) {
    if (val_or_none)
      attrs._css_map[key] = *val_or_none;
  }


  void set_attr(detail::StyleAttrs& attrs, const std::string& key,
                estd::optional<fo::LengthSpec> val_or_none) {
    if (val_or_none)
      attrs._css_map[key] = length_spec2css(*val_or_none);
  }


  void set_attr(detail::StyleAttrs& attrs, const std::string& key,
                const std::string& val) {
    attrs._css_map[key] = val;
  }


  void set_attr(detail::StyleAttrs& attrs, const std::string& key, int val) {
    attrs._css_map[key] = std::to_string(val);
  }


  const auto k_normal = std::string("normal");
  const auto k_lower = std::string("lower");
  const auto k_caps = std::string("caps");
  const auto k_small_caps = std::string("small-caps");


  void set_capsstyle(detail::StyleAttrs& attrs, estd::optional<std::string> val_or_none) {
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
                         estd::optional<std::string> val_or_none,
                         const std::string& exp_val) {
    if (val_or_none) {
      if (*val_or_none == exp_val)
        return {processor->ctx().port(), tag};
    }

    return {};
  }


  html::Attrs tag_style_attrs(HtmlProcessor* processor, const std::string& tag,
                              const detail::StyleAttrs& attrs) {
    auto str = attrs_to_string(attrs);
    if (!str.empty()) {
      auto cls = processor->css_writer().add_rule(tag, str);
      return {{"class", cls}};
    }

    return {};
  }


  html::Tag optional_span_tag(HtmlProcessor* processor, const detail::StyleAttrs& attrs) {
    if (!attrs._css_map.empty()) {
      return {processor->ctx().port(), "span", tag_style_attrs(processor, "span", attrs)};
    }

    return {};
  }


  html::Tag optional_inline_block_tag(HtmlProcessor* processor,
                                      const detail::StyleAttrs& attrs) {
    auto extAttrs = detail::StyleAttrs(attrs);
    extAttrs._css_map["display"] = "inline-block";
    extAttrs._css_map["text-indent"] = "0em";

    if (!extAttrs._css_map.empty()) {
      return {processor->ctx().port(), "span",
              tag_style_attrs(processor, "span", extAttrs)};
    }

    return {};
  }


  struct FontCharacteristics
  {
    estd::optional<std::string> _fontname;
    estd::optional<fo::LengthSpec> _fontsize;
    estd::optional<std::string> _fontcaps;
    estd::optional<fo::LengthSpec> _posptshift;
    estd::optional<std::string> _posture;
    estd::optional<std::string> _weight;
    estd::optional<fo::LengthSpec> _linespacing;
  };


  FontCharacteristics extract_font_characteristics(HtmlProcessor* processor,
                                                   const IFormattingObject* fo) {
    auto result = FontCharacteristics{};

    result._fontsize = processor->property_or_none<fo::LengthSpec>(fo, "font-size");
    result._fontcaps = processor->property_or_none<std::string>(fo, "font-caps");
    result._posptshift =
      processor->property_or_none<fo::LengthSpec>(fo, "position-point-shift");

    result._fontname = processor->property_or_none<std::string>(fo, "font-name");
    result._posture = processor->property_or_none<std::string>(fo, "font-posture");
    result._weight = processor->property_or_none<std::string>(fo, "font-weight");

    result._linespacing = processor->property_or_none<fo::LengthSpec>(fo, "line-spacing");

    return result;
  }

  void set_attr(detail::StyleAttrs& attrs, const FontCharacteristics& fontchar) {
    if (fontchar._fontname) {
      if (*fontchar._fontname == "monospace")
        set_attr(attrs, "font-family", "monospace");
      else if (*fontchar._fontname == "sans-serif")
        set_attr(attrs, "font-family", "sans-serif");
      else if (*fontchar._fontname == "roman")
        set_attr(attrs, "font-family", "serif");
      else
        set_attr(attrs, "font-family", std::string("'") + *fontchar._fontname + "'");
    }

    if (fontchar._weight) {
      if (*fontchar._weight == "thin")
        set_attr(attrs, "font-weight", 100);
      else if (*fontchar._weight == "light")
        set_attr(attrs, "font-weight", 300);
      else if (*fontchar._weight == "medium")
        set_attr(attrs, "font-weight", 400);
      else if (*fontchar._weight == "semibold")
        set_attr(attrs, "font-weight", 500);
      else if (*fontchar._weight == "bold")
        set_attr(attrs, "font-weight", 700);
      else if (*fontchar._weight == "black")
        set_attr(attrs, "font-weight", 900);
    }

    if (fontchar._posture) {
      set_attr(attrs, "font-style", *fontchar._posture);
    }

    set_attr(attrs, "font-size", fontchar._fontsize);
    set_capsstyle(attrs, fontchar._fontcaps);
    if (fontchar._posptshift && fontchar._posptshift->_value != 0) {
      set_attr(attrs, "position", "relative");
      set_attr(attrs, "top", fontchar._posptshift);
    }

    if (fontchar._linespacing && fontchar._linespacing->_value != 0)
      set_attr(attrs, "line-height", fontchar._linespacing);
  }


  struct SpaceCharacteristics
  {
    estd::optional<fo::LengthSpec> _startindent;
    estd::optional<fo::LengthSpec> _endindent;
    estd::optional<fo::LengthSpec> _firstline_startindent;
    estd::optional<fo::LengthSpec> _spacebefore;
    estd::optional<fo::LengthSpec> _spaceafter;
  };


  SpaceCharacteristics extract_space_characteristics(HtmlProcessor* processor,
                                                     const IFormattingObject* fo) {
    auto result = SpaceCharacteristics{};

    result._startindent = processor->property_or_none<fo::LengthSpec>(fo, "start-indent");
    result._endindent = processor->property_or_none<fo::LengthSpec>(fo, "end-indent");
    result._firstline_startindent =
      processor->property_or_none<fo::LengthSpec>(fo, "first-line-start-indent");
    result._spacebefore = processor->property_or_none<fo::LengthSpec>(fo, "space-before");
    result._spaceafter = processor->property_or_none<fo::LengthSpec>(fo, "space-after");

    return result;
  }


  void set_attr(detail::StyleAttrs& attrs, const SpaceCharacteristics& spacechar) {
    if (spacechar._startindent && spacechar._startindent->_value != 0)
      set_attr(attrs, "padding-left", spacechar._startindent);
    if (spacechar._endindent && spacechar._endindent->_value != 0)
      set_attr(attrs, "padding-right", spacechar._endindent);
    if (spacechar._spacebefore && spacechar._spacebefore->_value != 0) {
      if (spacechar._spacebefore->_conditionalp)
        set_attr(attrs, "margin-top", spacechar._spacebefore);
      else
        set_attr(attrs, "padding-top", spacechar._spacebefore);
    }
    if (spacechar._spaceafter && spacechar._spaceafter->_value != 0) {
      if (spacechar._spaceafter->_conditionalp)
        set_attr(attrs, "margin-bottom", spacechar._spaceafter);
      else
        set_attr(attrs, "padding-bottom", spacechar._spaceafter);
    }
    set_attr(attrs, "text-indent", spacechar._firstline_startindent);
  }


  struct MarginCharacteristics
  {
    estd::optional<fo::LengthSpec> _startmargin;
    estd::optional<fo::LengthSpec> _endmargin;
  };


  MarginCharacteristics extract_margin_characteristics(HtmlProcessor* processor,
                                                       const IFormattingObject* fo) {
    auto result = MarginCharacteristics{};

    result._startmargin = processor->property_or_none<fo::LengthSpec>(fo, "start-margin");
    result._endmargin = processor->property_or_none<fo::LengthSpec>(fo, "end-margin");

    return result;
  }


  void set_attr(detail::StyleAttrs& attrs, const MarginCharacteristics& marginchar) {
    set_attr(attrs, "margin-left", marginchar._startmargin);
    set_attr(attrs, "margin-right", marginchar._endmargin);
  }


  class StyleScope
  {
    detail::HtmlRenderContext& _ctx;

  public:
    StyleScope(detail::HtmlRenderContext& ctx, const detail::StyleAttrs& attrs)
      : _ctx(ctx) {
      _ctx.push_styles(attrs);
    }

    ~StyleScope() {
      _ctx.pop_styles();
    }
  };


  auto open_html_port(HtmlProcessor* processor, const fs::path& path,
                      const html::Doctype& doctype) -> fs::path {
    auto csspath = path;
    csspath.replace_extension("css");

    if (!processor->css_writer().is_open()) {
      if (processor->is_verbose()) {
        std::cout << processor->proc_id() << ": Create css file: '" << csspath << "'"
                  << std::endl;
      }

      processor->css_writer().open(csspath);
    }

    if (processor->is_verbose()) {
      std::cout << processor->proc_id() << ": Create output file: '" << path.string()
                << "'" << std::endl;
    }

    auto& ctx = processor->ctx();

    auto port = ::estd::make_unique<html::Writer>(doctype, k_TEXTBOOK_GENERATOR,
                                                  processor->style_ctx());

    port->open(path);

    ctx.push_port(std::move(port), path);

    return csspath;
  }


  void close_html_port(HtmlProcessor* processor) {
    processor->ctx().port().footer();
    processor->ctx().pop_port();
  }


  template <typename Functor>
  void with_html_file(HtmlProcessor* processor, const fs::path& path,
                      const std::string& title, const std::string& author,
                      const std::string& desc, const html::Doctype& doctype,
                      Functor functor) {
    auto csspath = open_html_port(processor, path, doctype);

    processor->ctx().port().header(title, author, desc, [&](std::ostream&) {
      processor->ctx().port().write_link("stylesheet",
                                         {{"media", "screen"},
                                          {"type", "text/css"},
                                          {"href", csspath.string()}});
    });

    functor(processor);

    close_html_port(processor);
  }


  class HtmlLiteralFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto str = static_cast<const fo::Literal*>(fo)->text();

      auto capsstyle = processor->ctx().capsstyle();
      if (capsstyle == detail::k_normal_caps) {
        processor->ctx().port().write_text(str);
      }
      else if (capsstyle == detail::k_lower_caps) {
        processor->ctx().port().write_text(utils::to_lower(str));
      }
      else if (capsstyle == detail::k_upper_caps) {
        processor->ctx().port().write_text(utils::to_upper(str));
      }
      else if (capsstyle == detail::k_small_caps) {
        processor->ctx().port().write_text(str);
      }
    }
  };


  class HtmlParagraphFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto attrs = detail::StyleAttrs{};

      set_attr(attrs, extract_font_characteristics(processor, fo));
      set_attr(attrs, extract_space_characteristics(processor, fo));

      auto quadding = processor->property(fo, "quadding", std::string("left"));
      if (quadding == "left")
        set_attr(attrs, "text-align", "left");
      else if (quadding == "right")
        set_attr(attrs, "text-align", "right");
      else if (quadding == "center")
        set_attr(attrs, "text-align", "center");
      else if (quadding == "justify")
        set_attr(attrs, "text-align", "justify");
      set_attr(attrs, "text-justify", "inter-word");

      auto color = processor->property_or_none<fo::Color>(fo, "color");
      if (color)
        set_attr(attrs, "color", enc_color(*color));

      auto bgcolor = processor->property_or_none<fo::Color>(fo, "background-color");
      if (bgcolor)
        set_attr(attrs, "background-color", enc_color(*bgcolor));

      auto& ctx = processor->ctx();

      {
        auto d_attrs = intersect_css_attrs(ctx, attrs);
        auto with_tag =
          html::Tag{ctx.port(), "p", tag_style_attrs(processor, "p", d_attrs)};
        auto style_scope = StyleScope{ctx, d_attrs};

        auto b_tag = html::Tag{
          optional_tag(processor, "b",
                       processor->property_or_none<std::string>(fo, "font-weight"),
                       "bold")};
        auto i_tag = html::Tag{
          optional_tag(processor, "i",
                       processor->property_or_none<std::string>(fo, "font-posture"),
                       "italic")};

        auto linesprops = processor->property(fo, "lines", std::string("wrap"));
        if (linesprops == "asis")
          processor->style_ctx()._wrapstyle = html::detail::k_asis_wrap;
        else
          processor->style_ctx()._wrapstyle = html::detail::k_normal_wrap;

        auto old_wstreatment = processor->style_ctx()._wstreatment;
        auto wstreatment =
          processor->property(fo, "whitespace-treatment", std::string("collapse"));
        if (wstreatment == "preserve")
          processor->style_ctx()._wstreatment = html::detail::k_preserve_ws;
        else if (wstreatment == "collapse")
          processor->style_ctx()._wstreatment = html::detail::k_collapse_ws;
        else if (wstreatment == "ignore")
          processor->style_ctx()._wstreatment = html::detail::k_ignore_ws;

        processor->render_sosofo(&fo->port(k_text));

        processor->style_ctx()._wstreatment = old_wstreatment;
      }

      ctx.port().newln();
    }
  };


  class HtmlParagraphBreakFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto& ctx = processor->ctx();
      ctx.port().empty_tag("br");
      ctx.port().newln();
    }
  };


  class HtmlDisplayGroupFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto attrs = detail::StyleAttrs{};

      set_attr(attrs, extract_font_characteristics(processor, fo));
      set_attr(attrs, extract_space_characteristics(processor, fo));

      auto& ctx = processor->ctx();

      {
        auto d_attrs = intersect_css_attrs(ctx, attrs);
        auto with_tag =
          html::Tag{ctx.port(), "div", tag_style_attrs(processor, "div", d_attrs)};
        auto style_scope = StyleScope{ctx, d_attrs};

        ctx.port().newln();
        processor->render_sosofo(&fo->port(k_text));
      }

      ctx.port().newln();
    }
  };


  class HtmlBoxFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto attrs = detail::StyleAttrs{};

      // set_attr(attrs, extract_font_characteristics(processor, fo));
      // set_attr(attrs, extract_space_characteristics(processor, fo));

      const auto is_display = processor->property(fo, "display?", false);
      const auto boxtype = processor->property(fo, "box-type", std::string("border"));
      const auto has_border = boxtype == "border" || boxtype == "both";
      const auto has_background = boxtype == "background" || boxtype == "both";

      if (has_border) {
        if (auto color = processor->property_or_none<fo::Color>(fo, "color"))
          set_attr(attrs, "border-color", enc_color(*color));
        else
          set_attr(attrs, "border-color", "black");

        if (auto thickness =
              processor->property_or_none<fo::LengthSpec>(fo, "line-thickness"))
          set_attr(attrs, "border", length_spec2css(*thickness) + " solid");
        else
          set_attr(attrs, "border", "1pt solid");
      }

      if (has_background) {
        if (auto bgcolor = processor->property_or_none<fo::Color>(fo, "background-color"))
          set_attr(attrs, "background-color", enc_color(*bgcolor));
      }

      if (processor->property(fo, "box-corner-rounded?", false)) {
        if (auto radius =
              processor->property_or_none<fo::LengthSpec>(fo, "box-corner-radius"))
          set_attr(attrs, "border-radius", radius);
        else
          set_attr(attrs, "border-radius", "3pt");
      }

      auto& ctx = processor->ctx();

      {
        const auto tag = is_display ? "div" : "span";

        auto d_attrs = intersect_css_attrs(ctx, attrs);
        auto with_tag =
          html::Tag{ctx.port(), tag, tag_style_attrs(processor, tag, d_attrs)};
        auto style_scope = StyleScope{ctx, d_attrs};

        if (is_display)
          ctx.port().newln();
        processor->render_sosofo(&fo->port(k_text));
      }

      if (is_display)
        ctx.port().newln();
    }
  };


  class HtmlScrollSequenceFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto title = processor->property(fo, "title", std::string());
      auto author = processor->property(fo, "metadata.author", std::string());
      auto desc = processor->property(fo, "metadata.desc", std::string());
      auto width = processor->property_or_none<fo::LengthSpec>(fo, "width");

      auto& ctx = processor->ctx();

      if (!ctx.port().has_header()) {
        ctx.port().header(title, author, desc, [&](std::ostream&) {
          ctx.port().write_link("stylesheet",
                                {{"media", "screen"},
                                 {"type", "text/css"},
                                 {"href", processor->css_file().string()}});
        });
      }

      auto attrs = detail::StyleAttrs{};
      set_attr(attrs, "width", width);

      auto color = processor->property_or_none<fo::Color>(fo, "color");
      if (color)
        set_attr(attrs, "color", enc_color(*color));

      auto bgcolor = processor->property_or_none<fo::Color>(fo, "background-color");
      if (bgcolor)
        set_attr(attrs, "background-color", enc_color(*bgcolor));

      set_attr(attrs, extract_margin_characteristics(processor, fo));

      {
        auto with_tag =
          html::Tag{ctx.port(), "div", tag_style_attrs(processor, "div", attrs)};
        auto style_scope = StyleScope{ctx, attrs};

        ctx.port().newln();

        processor->render_sosofo(&fo->port("scroll"));
      }

      ctx.port().newln();
    }
  };


  struct ScreenZoneSpec
  {
    std::string _portnm;
    bool _is_fixed = false;
    bool _full_height = false;
    estd::optional<fo::LengthSpec> _height;
    estd::optional<fo::LengthSpec> _width;
    estd::optional<fo::LengthSpec> _x_origin;
    estd::optional<fo::LengthSpec> _y_origin;
    estd::optional<fo::Color> _bg_color;
    FontCharacteristics _font_characteristics;
    MarginCharacteristics _margin_characteristics;
  };


  FontCharacteristics extract_font_characteristics(const fo::PropertySpecs& propspecs) {
    auto result = FontCharacteristics{};

    result._fontsize = propspecs.lookup_value<fo::LengthSpec>("font-size");
    result._fontcaps = propspecs.lookup_value<std::string>("font-caps");
    result._posptshift = propspecs.lookup_value<fo::LengthSpec>("position-point-shift");

    result._fontname = propspecs.lookup_value<std::string>("font-name");

    result._posture = propspecs.lookup_value<std::string>("font-posture");
    result._weight = propspecs.lookup_value<std::string>("font-weight");

    result._linespacing = propspecs.lookup_value<fo::LengthSpec>("line-spacing");

    return result;
  }


  MarginCharacteristics
  extract_margin_characteristics(const fo::PropertySpecs& propspecs) {
    auto result = MarginCharacteristics{};

    result._startmargin = propspecs.lookup_value<fo::LengthSpec>("start-margin");
    result._endmargin = propspecs.lookup_value<fo::LengthSpec>("end-margin");

    return result;
  }


  ScreenZoneSpec translate_screen_set_region(const std::string& default_portnm,
                                             const fo::ScreenSetRegion& region) {
    auto result = ScreenZoneSpec{};
    result._portnm = default_portnm;

    result._portnm = region._props.lookup_value_or("port", default_portnm);
    result._is_fixed = region._props.lookup_value_or("fixed?", false);
    result._height = region._props.lookup_value<fo::LengthSpec>("height");
    result._width = region._props.lookup_value<fo::LengthSpec>("width");
    result._bg_color = region._props.lookup_value<fo::Color>("background-color");
    result._x_origin = region._props.lookup_value<fo::LengthSpec>("x-origin");
    result._y_origin = region._props.lookup_value<fo::LengthSpec>("y-origin");
    result._full_height =
      region._props.lookup_value_or<bool>("span-screen-height?", false);
    result._font_characteristics = extract_font_characteristics(region._props);
    result._margin_characteristics = extract_margin_characteristics(region._props);

    return result;
  }


  class HtmlScreenSetFoProcessor : public IFoProcessor<HtmlProcessor>
  {
    enum ZoneType
    {
      k_top,
      k_left,
      k_main,
      k_right,
      k_bottom,
    };

  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto title = processor->property(fo, "title", std::string());
      auto author = processor->property(fo, "metadata.author", std::string());
      auto desc = processor->property(fo, "metadata.desc", std::string());
      auto links = processor->property_or_none<std::string>(fo, "html.add-css-link");
      auto& ctx = processor->ctx();

      if (!ctx.port().has_header()) {
        ctx.port().header(title, author, desc, [&](std::ostream&) {
          ctx.port().write_link("stylesheet",
                                {{"media", "screen"},
                                 {"type", "text/css"},
                                 {"href", processor->css_file().string()}});
          if (links) {
            ctx.port().write_link("stylesheet",
                                  {{"media", "screen"},
                                   {"type", "text/css"},
                                   {"href", *links}});
          }
        });
      }

      std::map<std::string, std::vector<ScreenZoneSpec>> zones;
      zones["top"] = {};
      zones["left"] = {};
      zones["main"] = {};
      zones["right"] = {};
      zones["bottom"] = {};

      if (auto compound_value =
            processor->property_or_none<
              std::shared_ptr<fo::ICompoundValue>>(fo, "screen-set-model")) {
        if (auto screen_set_model =
              std::dynamic_pointer_cast<fo::ScreenSetModel>(*compound_value)) {
          for (const auto& region : screen_set_model->_regions) {
            auto i_zone = zones.find(region._zone);
            if (i_zone != zones.end()) {
              i_zone->second.emplace_back(
                translate_screen_set_region(region._zone, region));
            }
          }
        }
      }

      auto fo_attrs = detail::StyleAttrs{};

      set_attr(fo_attrs, extract_font_characteristics(processor, fo));

      render_top_bottom_zones(processor, fo, zones["top"], k_top, fo_attrs);
      render_fixed_side_zones(processor, fo, zones["left"], k_left, fo_attrs);
      render_fixed_side_zones(processor, fo, zones["right"], k_right, fo_attrs);

      {
        auto cont_attrs = detail::StyleAttrs{};
        set_attr(cont_attrs, "left", fo::LengthSpec(fo::kDimen, 0, fo::k_pt));
        set_attr(cont_attrs, "width", "100%");
        if (!zones["main"].empty() && zones["main"].front()._y_origin) {
          set_attr(cont_attrs, "margin",
                   length_spec2css(*zones["main"].front()._y_origin) + " auto");
        }
        else if (!zones["top"].empty() && zones["top"].front()._height) {
          set_attr(cont_attrs, "margin",
                   length_spec2css(*zones["top"].front()._height) + " auto");

          auto top_offset = *zones["top"].front()._height;
          top_offset._value *= -1;
          processor->set_top_zone_offset(top_offset);
        }
        set_attr(cont_attrs, "padding", "0px");

        auto with_tag =
          html::Tag{ctx.port(), "div", tag_style_attrs(processor, "div", cont_attrs)};
        auto style_scope = StyleScope{ctx, cont_attrs};

        render_floating_side_zones(processor, fo, zones["left"], k_left, fo_attrs);

        for (const auto& zone : zones["main"]) {
          auto attrs = fo_attrs;

          set_attr(attrs, "display", "inline-block");
          set_attr(attrs, "vertical-align", "top");

          if (zone._width)
            set_attr(attrs, "width", *zone._width);

          if (zone._x_origin)
            set_attr(attrs, "left", *zone._x_origin);
          if (zone._y_origin)
            set_attr(attrs, "top", *zone._y_origin);
          else
            set_attr(attrs, "top", fo::LengthSpec(fo::kDimen, 0, fo::k_pt));

          if (zone._bg_color)
            set_attr(attrs, "background-color", enc_color(*zone._bg_color));

          render_port(processor, fo, zone._portnm, zone, attrs);
        }

        render_floating_side_zones(processor, fo, zones["right"], k_right, fo_attrs);
      }

      render_top_bottom_zones(processor, fo, zones["bottom"], k_bottom, fo_attrs);

      ctx.port().newln();
    }

  private:
    std::string zone_type_to_css_distance(ZoneType zone_type) const {
      switch (zone_type) {
      case k_left:
        return "left";
      case k_right:
        return "right";
      case k_top:
        return "top";
      case k_bottom:
        return "bottom";
      default:
        return "";
      }
    }

    void render_top_bottom_zones(HtmlProcessor* processor, const IFormattingObject* fo,
                                 const std::vector<ScreenZoneSpec>& zones,
                                 ZoneType zone_type,
                                 const detail::StyleAttrs& xattrs) const {
      for (const auto& zone : zones) {
        auto attrs = xattrs;

        set_attr(attrs, "display", "block");

        if (zone._width)
          set_attr(attrs, "width", *zone._width);
        else
          set_attr(attrs, "width", "100%");
        if (zone._height)
          set_attr(attrs, "height", *zone._height);

        if (zone._bg_color)
          set_attr(attrs, "background-color", enc_color(*zone._bg_color));

        if (zone._y_origin)
          set_attr(attrs, zone_type_to_css_distance(zone_type), *zone._y_origin);
        else
          set_attr(attrs, zone_type_to_css_distance(zone_type),
                   fo::LengthSpec(fo::kDimen, 0, fo::k_pt));

        if (zone._x_origin)
          set_attr(attrs, "left", *zone._x_origin);
        else
          set_attr(attrs, "left", fo::LengthSpec(fo::kDimen, 0, fo::k_pt));

        if (zone._is_fixed) {
          // set_attr(attrs, "position", "sticky");
          set_attr(attrs, "position", "fixed");
        }

        render_port(processor, fo, zone._portnm, zone, attrs);
      }
    }

    void render_fixed_side_zones(HtmlProcessor* processor, const IFormattingObject* fo,
                                 const std::vector<ScreenZoneSpec>& zones,
                                 ZoneType zone_type,
                                 const detail::StyleAttrs& xattrs) const {
      for (const auto& zone : zones) {
        if (zone._is_fixed) {
          auto attrs = xattrs;

          set_attr(attrs, "display", "block");
          set_attr(attrs, "position", "fixed");

          if (zone._width)
            set_attr(attrs, "width", *zone._width);
          if (zone._full_height) {
            if (zone._y_origin) {
              set_attr(attrs, "height",
                       std::string("calc(100% - ") + length_spec2css(*zone._y_origin) +
                         ")");
              set_attr(attrs, "overflow-y", "auto");
            }
            else
              set_attr(attrs, "height", "100%");
          }
          else if (zone._height)
            set_attr(attrs, "height", *zone._height);

          if (zone._y_origin)
            set_attr(attrs, "top", *zone._y_origin);

          if (zone._bg_color)
            set_attr(attrs, "background-color", enc_color(*zone._bg_color));

          if (zone._x_origin)
            set_attr(attrs, zone_type_to_css_distance(zone_type), *zone._x_origin);
          else
            set_attr(attrs, zone_type_to_css_distance(zone_type),
                     fo::LengthSpec(fo::kDimen, 0, fo::k_pt));

          render_port(processor, fo, zone._portnm, zone, attrs);
        }
      }
    }

    void render_floating_side_zones(HtmlProcessor* processor, const IFormattingObject* fo,
                                    const std::vector<ScreenZoneSpec>& zones,
                                    ZoneType zone_type,
                                    const detail::StyleAttrs& xattrs) const {
      for (const auto& zone : zones) {
        auto attrs = xattrs;
        set_attr(attrs, "float", zone_type_to_css_distance(zone_type));

        if (zone._width)
          set_attr(attrs, "width", *zone._width);
        if (zone._full_height) {
          if (zone._y_origin)
            set_attr(attrs, "height",
                     std::string("calc(100% - ") + length_spec2css(*zone._y_origin) +
                       ")");
          else
            set_attr(attrs, "height", "100%");
        }
        else if (zone._height)
          set_attr(attrs, "height", *zone._height);

        if (!zone._is_fixed) {
          if (zone._bg_color)
            set_attr(attrs, "background-color", enc_color(*zone._bg_color));
          render_port(processor, fo, zone._portnm, zone, attrs);
        }
        else {
          auto& ctx = processor->ctx();
          auto with_tag =
            html::Tag{ctx.port(), "div", tag_style_attrs(processor, "div", attrs)};
          ctx.port().write_text("&nbsp;");
        }
      }
    }

    void render_port(HtmlProcessor* processor, const IFormattingObject* fo,
                     const std::string& portnm, const ScreenZoneSpec& zone,
                     const detail::StyleAttrs& attrs) const {
      auto& ctx = processor->ctx();

      const auto area_sosofo = fo->port(portnm);
      if (!area_sosofo.empty()) {
        {
          auto with_tag =
            html::Tag{ctx.port(), "div", tag_style_attrs(processor, "div", attrs)};

          {
            auto attrs2 = detail::StyleAttrs{};
            set_attr(attrs2, zone._font_characteristics);
            set_attr(attrs2, zone._margin_characteristics);

            auto dg_attrs = intersect_css_attrs(ctx, attrs2);
            auto with_tag =
              html::Tag{ctx.port(), "div", tag_style_attrs(processor, "div", dg_attrs)};
            auto style_scope = StyleScope{ctx, dg_attrs};

            ctx.port().newln();
            processor->render_sosofo(&area_sosofo);
          }
        }
        ctx.port().newln();
      }
    }
  };


  class HtmlSequenceFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto attrs = detail::StyleAttrs{};
      set_attr(attrs, extract_font_characteristics(processor, fo));

      {
        auto& ctx = processor->ctx();

        auto d_attrs = intersect_css_attrs(ctx, attrs);
        auto span_tag = html::Tag{optional_span_tag(processor, d_attrs)};
        auto style_scope = StyleScope{ctx, d_attrs};

        auto b_tag = html::Tag{
          optional_tag(processor, "b",
                       processor->property_or_none<std::string>(fo, "font-weight"),
                       "bold")};
        auto i_tag = html::Tag{
          optional_tag(processor, "i",
                       processor->property_or_none<std::string>(fo, "font-posture"),
                       "italic")};

        processor->render_sosofo(&fo->port(k_text));
      }
    }
  };


  const auto k_left = std::string("left");
  const auto k_center = std::string("center");
  const auto k_right = std::string("right");

  class HtmlLineFieldFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto attrs = detail::StyleAttrs{};
      set_attr(attrs, extract_font_characteristics(processor, fo));

      auto field_width = processor->property_or_none<fo::LengthSpec>(fo, "field-width");
      auto field_align = processor->property_or_none<std::string>(fo, "field-align");

      if (field_width && field_width->_value > 0) {
        const auto max_inf = std::numeric_limits<double>::infinity();

        if (field_width->_max == max_inf)
          set_attr(attrs, "min-width", field_width);
        else
          set_attr(attrs, "width", field_width);
      }
      if (field_align) {
        if (*field_align == k_left || *field_align == k_center ||
            *field_align == k_right) {
          set_attr(attrs, "text-align", *field_align);
        }
      }

      {
        auto d_attrs = intersect_css_attrs(processor->ctx(), attrs);
        auto span_tag =
          html::Tag{field_width ? optional_inline_block_tag(processor, d_attrs)
                                : optional_span_tag(processor, d_attrs)};
        auto style_scope = StyleScope{processor->ctx(), d_attrs};

        auto b_tag = html::Tag{
          optional_tag(processor, "b",
                       processor->property_or_none<std::string>(fo, "font-weight"),
                       "bold")};
        auto i_tag = html::Tag{
          optional_tag(processor, "i",
                       processor->property_or_none<std::string>(fo, "font-posture"),
                       "italic")};

        processor->render_sosofo(&fo->port(k_text));
      }
    }
  };


  class HtmlPageNumberFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      auto refid = processor->property(fo, "refid", std::string("#current"));
      if (refid == "#current") {
      }
      else if (!refid.empty()) {
        auto with_tag =
          html::Tag{processor->ctx().port(), "a", {{"href", std::string("#") + refid}}};
        processor->ctx().port().write_text("*");
      }
    }
  };


  class HtmlScoreFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* po, const IFormattingObject* fo) const override {
      auto attrs = detail::StyleAttrs{};

      const auto color = po->property_or_none<fo::Color>(fo, "color");
      const auto thickness = po->property_or_none<fo::LengthSpec>(fo, "line-thickness");
      const auto type = po->property(fo, "score-type", std::string("none"));

      auto border_val = std::string{};
      if (thickness)
        border_val += length_spec2css(*thickness);
      else
        border_val += "1px";

      if (color)
        border_val += " solid " + enc_color(*color);
      else
        border_val += " solid black";

      if (type == "above") {
        set_attr(attrs, "text-decoration", "none");
        set_attr(attrs, "border-top", border_val);
      }
      else if (type == "below") {
        set_attr(attrs, "text-decoration", "none");
        set_attr(attrs, "border-bottom", border_val);
      }
      else if (type == "through") {
        // there's no way to control the thickness of line-through
        set_attr(attrs, "text-decoration", "line-through");
      }

      auto& ctx = po->ctx();
      auto with_tag = html::Tag{ctx.port(), "span", tag_style_attrs(po, "span", attrs)};
      auto style_scope = StyleScope{ctx, attrs};

      po->render_sosofo(&fo->port(k_text));
    }
  };


  class HtmlLinkFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* po, const IFormattingObject* fo) const override {
      if (auto adr = po->property_or_none<fo::Address>(fo, "destination")) {
        auto sattrs = detail::StyleAttrs{};

        if (auto color = po->property_or_none<fo::Color>(fo, "color"))
          set_attr(sattrs, "color", enc_color(*color));
        else
          set_attr(sattrs, "color", "black");

        const auto type = po->property(fo, "score-type", std::string("none"));

        if (type == "above")
          set_attr(sattrs, "text-decoration", "overline");
        else if (type == "below")
          set_attr(sattrs, "text-decoration", "underline");
        else if (type == "through")
          set_attr(sattrs, "text-decoration", "line-through");
        else
          set_attr(sattrs, "text-decoration", "none");

        const auto href =
          adr->_local ? std::string("#") + adr->_destination : adr->_destination;

        auto attrs = tag_style_attrs(po, "a", sattrs);
        attrs.emplace_back(html::Attr{"href", href});

        auto& ctx = po->ctx();
        auto with_tag = html::Tag{ctx.port(), "a", attrs};
        auto style_scope = StyleScope{ctx, sattrs};

        po->render_sosofo(&fo->port(k_text));
      }
    }
  };


  class HtmlAnchorFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      if (auto id = processor->property_or_none<std::string>(fo, "id")) {
        if (!id->empty()) {
          auto sattrs = detail::StyleAttrs{};
          if (processor->top_zone_offset()) {
            set_attr(sattrs, "display", "block");
            set_attr(sattrs, "position", "relative");
            set_attr(sattrs, "top", length_spec2css(*processor->top_zone_offset()));
            set_attr(sattrs, "visibility", "hidden");
          }

          auto attrs = tag_style_attrs(processor, "a", sattrs);
          attrs.emplace_back(html::Attr{"id", *id});

          auto& ctx = processor->ctx();
          auto with_tag = html::Tag{ctx.port(), "a", attrs};
        }
      }
    }
  };


  class HtmlSimpleColumnSetSequenceProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* po, const IFormattingObject* fo) const override {
      auto attrs = detail::StyleAttrs{};
      auto col_num = po->property(fo, "column-number", 1);
      set_attr(attrs, "column-count", col_num);

      auto& ctx = po->ctx();

      auto with_tag = html::Tag{ctx.port(), "div", tag_style_attrs(po, "div", attrs)};
      auto style_scope = StyleScope{ctx, attrs};

      po->render_sosofo(&fo->port(k_text));
    }
  };
} // ns anon


HtmlProcessor::HtmlProcessor()
  : _verbose(false)
  , _css_port(k_TEXTBOOK_GENERATOR) {}


HtmlProcessor::HtmlProcessor(const po::variables_map& args)
  : HtmlProcessor() {
  if (!args.empty())
    _verbose = args.count("verbose") != 0;
}


po::options_description HtmlProcessor::program_options() const {
  auto opts_title = std::string("HTML renderer [selector: '") + proc_id() + "']";
  auto desc = po::options_description(opts_title);

  return desc;
}


const IFoProcessor<HtmlProcessor>*
HtmlProcessor::lookup_fo_processor(const std::string& fo_classname) const {
  static auto procs = std::map<std::string, std::shared_ptr<IFoProcessor<HtmlProcessor>>>{
    {"#literal", std::make_shared<HtmlLiteralFoProcessor>()},
    {"#paragraph", std::make_shared<HtmlParagraphFoProcessor>()},
    {"#paragraph-break", std::make_shared<HtmlParagraphBreakFoProcessor>()},
    {"#display-group", std::make_shared<HtmlDisplayGroupFoProcessor>()},
    {"#box", std::make_shared<HtmlBoxFoProcessor>()},
    {"#scroll-sequence", std::make_shared<HtmlScrollSequenceFoProcessor>()},
    {"#screen-set", std::make_shared<HtmlScreenSetFoProcessor>()},
    {"#sequence", std::make_shared<HtmlSequenceFoProcessor>()},
    {"#line-field", std::make_shared<HtmlLineFieldFoProcessor>()},
    {"#anchor", std::make_shared<HtmlAnchorFoProcessor>()},
    {"#page-number", std::make_shared<HtmlPageNumberFoProcessor>()},
    {"#link", std::make_shared<HtmlLinkFoProcessor>()},
    {"#score", std::make_shared<HtmlScoreFoProcessor>()},
    {"#simple-column-set-sequence",
     std::make_shared<HtmlSimpleColumnSetSequenceProcessor>()},
  };

  auto i_find = procs.find(fo_classname);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void HtmlProcessor::before_rendering() {
  _css_file = open_html_port(this, _output_file, html::k_XHTML_1_1_DTD);
}


void HtmlProcessor::after_rendering() {
  close_html_port(this);
}

} // ns eyestep
