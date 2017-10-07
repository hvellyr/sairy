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
    auto rco = [&]() -> std::tuple<int, int, int> {
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
    ss << "#" << std::hex << std::get<0>(rco) << std::get<1>(rco) << std::get<2>(rco);

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


  void set_font_characteristics(detail::StyleAttrs& attrs, HtmlProcessor* processor,
                                const IFormattingObject* fo) {
    auto fontsize = processor->property_or_none<fo::LengthSpec>(fo, "font-size");
    auto fontcaps = processor->property_or_none<std::string>(fo, "font-caps");
    auto posptshift =
      processor->property_or_none<fo::LengthSpec>(fo, "position-point-shift");

    if (auto fn = processor->property_or_none<std::string>(fo, "font-name")) {
      if (*fn == "monospace")
        set_attr(attrs, "font-family", "monospace");
      else if (*fn == "sans-serif")
        set_attr(attrs, "font-family", "sans-serif");
      else if (*fn == "roman")
        set_attr(attrs, "font-family", "serif");
    }

    set_attr(attrs, "font-size", fontsize);
    set_capsstyle(attrs, fontcaps);
    if (posptshift && posptshift->_value != 0) {
      set_attr(attrs, "position", "relative");
      set_attr(attrs, "top", posptshift);
    }
  }


  void set_space_characteristics(detail::StyleAttrs& attrs, HtmlProcessor* processor,
                                 const IFormattingObject* fo) {
    auto startindent = processor->property_or_none<fo::LengthSpec>(fo, "start-indent");
    auto endindent = processor->property_or_none<fo::LengthSpec>(fo, "end-indent");
    auto firstline_startindent =
      processor->property_or_none<fo::LengthSpec>(fo, "first-line-start-indent");
    auto spacebefore = processor->property_or_none<fo::LengthSpec>(fo, "space-before");
    auto spaceafter = processor->property_or_none<fo::LengthSpec>(fo, "space-after");

    if (startindent && startindent->_value != 0)
      set_attr(attrs, "padding-left", startindent);
    if (endindent && endindent->_value != 0)
      set_attr(attrs, "margin-right", endindent);
    if (spacebefore && spacebefore->_value != 0)
      set_attr(attrs, "margin-top", spacebefore);
    if (spaceafter && spaceafter->_value != 0)
      set_attr(attrs, "margin-bottom", spaceafter);
    set_attr(attrs, "text-indent", firstline_startindent);
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

      set_font_characteristics(attrs, processor, fo);
      set_space_characteristics(attrs, processor, fo);

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

        processor->render_sosofo(&fo->port("text"));

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

      set_font_characteristics(attrs, processor, fo);
      set_space_characteristics(attrs, processor, fo);

      auto& ctx = processor->ctx();

      {
        auto d_attrs = intersect_css_attrs(ctx, attrs);
        auto with_tag =
          html::Tag{ctx.port(), "div", tag_style_attrs(processor, "div", d_attrs)};
        auto style_scope = StyleScope{ctx, d_attrs};

        ctx.port().newln();
        processor->render_sosofo(&fo->port("text"));
      }

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

      auto start_margin = processor->property_or_none<fo::LengthSpec>(fo, "start-margin");
      set_attr(attrs, "margin-left", start_margin);
      auto end_margin = processor->property_or_none<fo::LengthSpec>(fo, "end-margin");
      set_attr(attrs, "margin-right", end_margin);

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


  class HtmlSequenceFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      detail::StyleAttrs attrs;
      set_font_characteristics(attrs, processor, fo);

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

        processor->render_sosofo(&fo->port("text"));
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
      detail::StyleAttrs attrs;
      set_font_characteristics(attrs, processor, fo);

      auto field_width = processor->property_or_none<fo::LengthSpec>(fo, "field-width");
      auto field_align = processor->property_or_none<std::string>(fo, "field-align");

      if (field_width && field_width->_value > 0) {
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

        processor->render_sosofo(&fo->port("text"));
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


  class HtmlAnchorFoProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* processor, const IFormattingObject* fo) const override {
      if (auto id = processor->property_or_none<std::string>(fo, "id")) {
        if (!id->empty())
          processor->ctx().port().empty_tag("a", {{"id", *id}});
      }
    }
  };


  class HtmlSimpleColumnSetSequenceProcessor : public IFoProcessor<HtmlProcessor>
  {
  public:
    void render(HtmlProcessor* po, const IFormattingObject* fo) const override {
      detail::StyleAttrs attrs;
      auto col_num = po->property(fo, "column-number", 1);
      set_attr(attrs, "column-count", col_num);

      auto& ctx = po->ctx();

      auto with_tag = html::Tag{ctx.port(), "div", tag_style_attrs(po, "div", attrs)};
      auto style_scope = StyleScope{ctx, attrs};

      po->render_sosofo(&fo->port("text"));
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
    {"#scroll-sequence", std::make_shared<HtmlScrollSequenceFoProcessor>()},
    {"#sequence", std::make_shared<HtmlSequenceFoProcessor>()},
    {"#line-field", std::make_shared<HtmlLineFieldFoProcessor>()},
    {"#anchor", std::make_shared<HtmlAnchorFoProcessor>()},
    {"#page-number", std::make_shared<HtmlPageNumberFoProcessor>()},
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
