// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "fos.hpp"
#include "estd/memory.hpp"
#include "fo.hpp"
#include "sosofo.hpp"

#include <algorithm>
#include <functional>
#include <iostream>
#include <limits>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>


namespace eyestep {

namespace fo {

  struct PropertyDefs
  {
    const std::string _key;
    fo::ValueType::Kind _type;
    const bool _is_inherited;
  };


  const auto s_properties = std::vector<PropertyDefs>{
    // clang-format off
// inherited:
    {"above?",                  ValueType::k_bool,   true},
    {"asis-wrap-indent",        ValueType::k_length, true},
    {"background-color",        ValueType::k_color,  true},
    {"background-tile",         ValueType::k_string, true}, // String: path to ext. graphics
    {"below?",                  ValueType::k_bool,   true},
    {"bottom-margin",           ValueType::k_length, true},
    {"box-corner-radius",       ValueType::k_length, true},
    {"box-corner-rounded?",     ValueType::k_bool,   true},
    {"class",                   ValueType::k_string, true},
    {"color",                   ValueType::k_color,  true},
    {"end-indent",              ValueType::k_length, true},
    {"end-margin",              ValueType::k_length, true},
    {"field-align",             ValueType::k_string, true}, // Keyw: left, right, center
    {"field-width",             ValueType::k_length, true},
    {"first-line-start-indent", ValueType::k_length, true},
    {"font-caps",               ValueType::k_string, true}, // Keyw: normal, caps, smallcaps,
    {"font-name",               ValueType::k_string, true},
    {"font-posture",            ValueType::k_string, true}, // Keyw: upright, italic, oblique
    {"font-size",               ValueType::k_length, true},
    {"font-weight",             ValueType::k_string, true}, // Keyw: medium, bold, semibold,
    {"footer-margin",           ValueType::k_length, true},
    {"gutter-width",            ValueType::k_length, true},
    {"header-margin",           ValueType::k_length, true},
    {"inhibit-line-breaks?",    ValueType::k_bool,   true},
    {"language",                ValueType::k_string, true},
    {"last-line-end-indent",    ValueType::k_length, true},
    {"left-margin",             ValueType::k_length, true},
    {"line-number-side",        ValueType::k_string, true}, // Keyw: start, end, inside, outside
    {"line-spacing",            ValueType::k_length, true},
    {"line-thickness",          ValueType::k_length, true},
    {"lines",                   ValueType::k_string, true}, // Keyw: wrap, asis, asis-wrap, none
    {"numbered-lines?",         ValueType::k_bool,   true},
    {"page-height",             ValueType::k_length, true},
    {"page-width",              ValueType::k_length, true},
    {"position-point-shift",    ValueType::k_length, true},
    {"quadding",                ValueType::k_string, true}, // Keyw: left, right, center, justify
    {"right-margin",            ValueType::k_length, true},
    {"start-indent",            ValueType::k_length, true},
    {"start-margin",            ValueType::k_length, true},
    {"text",                    ValueType::k_string, true},
    {"top-margin",              ValueType::k_length, true},
    {"whitespace-treatment",    ValueType::k_string, true}, // Keyw: preserve, collapse, ignore

// not inherited
    {"box-type",                ValueType::k_string, false}, // Keyw: border, background, both
    {"break-after",             ValueType::k_bool,   false}, // false or keyw: page
    {"break-before",            ValueType::k_bool,   false}, // false or keyw: page
    {"center-footer",           ValueType::k_sosofo, false},
    {"center-header",           ValueType::k_sosofo, false},
    {"column-number",           ValueType::k_int,    false},
    {"destination",             ValueType::k_address, false},
    {"display?",                ValueType::k_bool,   false},
    {"external-path",           ValueType::k_string, false},
    {"height",                  ValueType::k_length, false},
    {"id",                      ValueType::k_string, false},
    {"keep-with-next?",         ValueType::k_bool,   false},
    {"keep-with-previous?",     ValueType::k_bool,   false},
    {"left-footer",             ValueType::k_sosofo, false},
    {"left-header",             ValueType::k_sosofo, false},
    {"refid",                   ValueType::k_string, false},
    {"right-footer",            ValueType::k_sosofo, false},
    {"right-header",            ValueType::k_sosofo, false},
    {"score-type",              ValueType::k_string, false}, // Symbol: none, above, through, below
    {"space-after",             ValueType::k_length, false},
    {"space-before",            ValueType::k_length, false},
    {"title",                   ValueType::k_sosofo, false},
    {"width",                   ValueType::k_length, false},
    // clang-format on
  };


  const auto k_bottom = std::string("bottom");
  const auto k_left = std::string("left");
  const auto k_main = std::string("main");
  const auto k_right = std::string("right");
  const auto k_scroll = std::string("scroll");
  const auto k_text = std::string("text");
  const auto k_top = std::string("top");


  //----------------------------------------------------------------------------

  static Sosofo k_nil_sosofo;


  const PropertySpecs& Fo::default_properties() const {
    static const auto propspecs = PropertySpecs{};
    return propspecs;
  }


  const std::vector<std::string>& Fo::ports() const {
    static const auto ports = std::vector<std::string>{};
    return ports;
  }


  const Sosofo& Fo::port(const std::string&) const {
    return k_nil_sosofo;
  }


  void Fo::set_port(const std::string&, const Sosofo&) {}


  //----------------------------------------------------------------------------

  const PropertySpecs& Literal::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"text", ""},
      {"language", ""},
    };

    return propspecs;
  }


  const Sosofo& Literal::port(const std::string& /*portName*/) const {
    return k_nil_sosofo;
  }


  std::string Literal::text() const {
    if (auto spec = _props.lookup_key(k_text)) {
      if (const auto* val = fo::get<const std::string>(&spec->_value)) {
        return *val;
      }
    }

    return "";
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& ExternalGraphic::default_properties() const {
    static const auto propspecs = PropertySpecs{
      // clang-format off
      {"display?", true},
      {"external-path", ""},
      {"language", ""},
      {"height", false},
      {"width", false},
      {"keep-with-previous?", false},
      {"keep-with-next?", false},
      {"space-before", LengthSpec(kDisplay, 0, k_pt)},
      {"space-after", LengthSpec(kDisplay, 0, k_pt)},
      {"break-before", false},
      {"break-after", false},
      // clang-format off
    };

    return propspecs;
  }

  const Sosofo& ExternalGraphic::port(const std::string& /*portname*/) const {
    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  bool Paragraph::accepts_fo(const Sosofo& sosofo) const {
    using namespace std;

    return none_of(begin(sosofo), end(sosofo), [](const IFormattingObject& fo) {
      return dynamic_cast<const Paragraph*>(&fo) != nullptr &&
             dynamic_cast<const DisplayGroup*>(&fo) != nullptr;
    });
  }


  const PropertySpecs& Paragraph::default_properties() const {
    auto max_inf = std::numeric_limits<double>::infinity();

    static const auto propspecs = PropertySpecs{
      // clang-format off
      {"first-line-start-indent", LengthSpec(kInline, 0, k_pt)},
      {"last-line-end-indent", LengthSpec(kInline, 10, k_pt, 1, max_inf)},
      {"line-spacing", LengthSpec(kDimen, 14, k_pt)},
      {"font-caps", "normal"},
      {"font-name", "serif"},
      {"font-posture", "upright"},
      {"font-size", LengthSpec(kDimen, 10, k_pt)},
      {"font-weight", "medium"},
      {"language", ""},
      {"start-indent", LengthSpec(kInline, 0, k_pt)},
      {"end-indent", LengthSpec(kInline, 0, k_pt)},
      {"quadding", "justify"},
      {"space-before", LengthSpec(kDisplay, 0, k_pt)},
      {"space-after", LengthSpec(kDisplay, 0, k_pt)},
      {"keep-with-previous?", false},
      {"keep-with-next?", false},
      {"break-after", false},
      {"break-before", false},
      {"lines", "wrap"},
      {"whitespace-treatment", "collapse"},
      {"asis-wrap-indent", 10},
      {"numbered-lines?", false},
      {"line-number-side", "start"},
      {"position-point-shift", LengthSpec(kDimen, 0, k_pt)},
      {"color", ""},
      {"background-color", ""},
      // clang-format on
    };
    return propspecs;
  }


  const std::vector<std::string>& Paragraph::ports() const {
    static const auto ports = std::vector<std::string>{
      k_text,
    };
    return ports;
  }


  const Sosofo& Paragraph::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& DisplayGroup::default_properties() const {
    static const auto propspecs = PropertySpecs{
      // clang-format off
      {"space-before", LengthSpec(kDisplay, 0, k_pt)},
      {"space-after", LengthSpec(kDisplay, 0, k_pt)},
      {"break-before", false},
      {"break-after", false},
      {"font-caps", "normal"},
      {"font-name", "serif"},
      {"font-posture", "upright"},
      {"font-size", LengthSpec(kDimen, 10, k_pt)},
      {"font-weight", "medium"},
      {"lines", "wrap"},
      {"color", ""},
      {"start-indent", LengthSpec(kInline, 0, k_pt)},
      {"end-indent", LengthSpec(kInline, 0, k_pt)},
      {"background-color", ""},
      {"keep-with-previous?", false},
      {"keep-with-next?", false},
      // clang-format on
    };
    return propspecs;
  }

  const std::vector<std::string>& DisplayGroup::ports() const {
    static const auto ports = std::vector<std::string>{
      k_text,
    };
    return ports;
  }

  const Sosofo& DisplayGroup::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& Box::default_properties() const {
    // clang-format off
    static const auto propspecs = PropertySpecs{
      {"box-type", "border"},
      {"display?", true},
      {"box-corner-rounded?", false},
      {"box-corner-radius", LengthSpec(kDisplay, 3, k_pt)},
      {"background-color", ""},
      {"color", ""},
      {"space-before", LengthSpec(kDisplay, 0, k_pt)},
      {"space-after", LengthSpec(kDisplay, 0, k_pt)},
      {"break-before", false},
      {"break-after", false},
      {"line-thickness", LengthSpec(kDimen, 1, k_pt)},
    };
    // clang-format on
    return propspecs;
  }

  const std::vector<std::string>& Box::ports() const {
    static const auto ports = std::vector<std::string>{
      k_text,
    };
    return ports;
  }

  const Sosofo& Box::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& Sequence::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"font-caps", "normal"},
      {"font-name", "serif"},
      {"font-posture", "upright"},
      {"font-size", LengthSpec(kDimen, 10, k_pt)},
      {"font-weight", "medium"},
      {"position-point-shift", LengthSpec(kDimen, 0, k_pt)},
      {"color", ""},
    };
    return propspecs;
  }


  const std::vector<std::string>& Sequence::ports() const {
    static const auto ports = std::vector<std::string>{
      k_text,
    };
    return ports;
  }


  const Sosofo& Sequence::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& LineField::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"field-width", LengthSpec(kInline, 0, k_pt)},
      {"field-align", "left"},
      {"inhibit-line-breaks?", false},
      {"position-point-shift", LengthSpec(kDimen, 0, k_pt)},
      {"font-caps", "normal"},
      {"font-name", "serif"},
      {"font-posture", "upright"},
      {"font-size", LengthSpec(kDimen, 10, k_pt)},
      {"font-weight", "medium"},
    };
    return propspecs;
  }


  const std::vector<std::string>& LineField::ports() const {
    static const auto ports = std::vector<std::string>{
      k_text,
    };
    return ports;
  }


  const Sosofo& LineField::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& Score::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"score-type", "none"},
      {"color", ""},
      {"line-thickness", LengthSpec(kDimen, 1, k_pt)},
    };
    return propspecs;
  }


  const std::vector<std::string>& Score::ports() const {
    static const auto ports = std::vector<std::string>{
      k_text,
    };
    return ports;
  }


  const Sosofo& Score::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& SimplePageSequence::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"font-caps", "normal"},
      {"font-name", "serif"},
      {"font-posture", "upright"},
      {"font-size", LengthSpec(kDimen, 10, k_pt)},
      {"font-weight", "medium"},
      {"lines", "wrap"},
      {"whitespace-treatment", "collapse"},
      {"start-margin", LengthSpec(kInline, 0, k_pt)},
      {"end-margin", LengthSpec(kInline, 0, k_pt)},
      {"page-width", LengthSpec(kDimen, 210, k_mm)},
      {"page-height", LengthSpec(kDimen, 297, k_mm)},
      {"left-margin", LengthSpec(kInline, 30, k_mm)},
      {"right-margin", LengthSpec(kDimen, 30, k_mm)},
      {"top-margin", LengthSpec(kDimen, 20, k_mm)},
      {"bottom-margin", LengthSpec(kDimen, 30, k_mm)},
      {"header-margin", LengthSpec(kDimen, 10, k_mm)},
      {"footer-margin", LengthSpec(kDimen, 20, k_mm)},
      {"left-header", std::make_shared<Sosofo>()},
      {"center-header", std::make_shared<Sosofo>()},
      {"right-header", std::make_shared<Sosofo>()},
      {"left-footer", std::make_shared<Sosofo>()},
      {"center-footer", std::make_shared<Sosofo>()},
      {"right-footer", std::make_shared<Sosofo>()},
    };

    return propspecs;
  }


  const std::vector<std::string>& SimplePageSequence::ports() const {
    static const auto ports = std::vector<std::string>{
      k_text,
    };
    return ports;
  }


  const Sosofo& SimplePageSequence::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& SimpleColumnSetSequence::default_properties() const {
    static const auto propspecs = PropertySpecs{
      // clang-format off
      {"space-before", LengthSpec(kDisplay, 0, k_pt)},
      {"space-after", LengthSpec(kDisplay, 0, k_pt)},
      {"keep-with-previous?", false},
      {"keep-with-next?", false},
      {"break-after", false},
      {"break-before", false},
      {"column-number", 1},
      {"gutter-width", LengthSpec(kDimen, 21, k_pt)},
      // clang-format on
    };

    return propspecs;
  }


  const std::vector<std::string>& SimpleColumnSetSequence::ports() const {
    static const auto ports = std::vector<std::string>{
      k_text,
    };
    return ports;
  }


  const Sosofo& SimpleColumnSetSequence::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& ScrollSequence::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"font-caps", "normal"},
      {"font-name", "serif"},
      {"font-posture", "upright"},
      {"font-size", LengthSpec(kDimen, 10, k_pt)},
      {"font-weight", "medium"},
      {"title", false},
      {"width", LengthSpec(kDimen, 600, k_px)},
      {"start-margin", LengthSpec(kDimen, 0, k_pt)},
      {"end-margin", LengthSpec(kDimen, 0, k_pt)},
      {"background-color", false},
      {"background-tile", false},
    };

    return propspecs;
  }


  const std::vector<std::string>& ScrollSequence::ports() const {
    static const auto ports = std::vector<std::string>{k_scroll};
    return ports;
  }


  const Sosofo& ScrollSequence::port(const std::string& portname) const {
    if (portname == k_scroll)
      return _scroll_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& PageNumber::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"refid", "#current"},
    };
    return propspecs;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& Link::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"destination", false},
      {"color", ""},
      {"score-type", "none"},
      {"line-thickness", LengthSpec(kDimen, 1, k_pt)},
    };
    return propspecs;
  }


  const std::vector<std::string>& Link::ports() const {
    static const auto ports = std::vector<std::string>{k_text};
    return ports;
  }


  const Sosofo& Link::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& Anchor::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"display?", false},
      {"id", false},
    };
    return propspecs;
  }


  //----------------------------------------------------------------------------

  const PropertySpecs& FootNote::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"id", false},
    };
    return propspecs;
  }


  const std::vector<std::string>& FootNote::ports() const {
    static const auto ports = std::vector<std::string>{
      k_text,
    };
    return ports;
  }


  const Sosofo& FootNote::port(const std::string& portname) const {
    if (portname == k_text)
      return _text_port;

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  ScreenSet::ScreenSet(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props)
    , _port_names({k_main})
    , _ports({{k_main, sosofo}}) {}


  const PropertySpecs& ScreenSet::default_properties() const {
    static const auto propspecs = PropertySpecs{{"title", false},
                                                {"screen-set-model", false},
                                                {"html.add-css-link", false}};

    return propspecs;
  }


  const std::vector<std::string>& ScreenSet::ports() const {
    lookup_ports();
    return _port_names;
  }


  const Sosofo& ScreenSet::port(const std::string& portnm) const {
    using namespace std;

    lookup_ports();

    if (any_of(begin(_port_names), end(_port_names),
               [&](const auto& nm) { return portnm == nm; })) {
      auto i_port = _ports.find(portnm);
      if (i_port != end(_ports))
        return i_port->second;
    }

    return k_nil_sosofo;
  }


  void ScreenSet::set_port(const std::string& portnm, const Sosofo& sosofo) {
    using namespace std;

    if (portnm != k_main) {
      _ports[portnm] = sosofo;
    }
  }


  ScreenSetModel* ScreenSet::screen_set_model() const {
    if (!_screen_set) {
      if (auto prop = properties().lookup_key("screen-set-model")) {
        ValueType valv;

        if (auto* expr = fo::get<std::shared_ptr<fo::IExpr>>(&prop->_value)) {
          valv = expr->get()->eval(nullptr);
        }
        else
          valv = prop->_value;

        if (auto compval = fo::get<std::shared_ptr<fo::ICompoundValue>>(&valv)) {
          _screen_set = std::dynamic_pointer_cast<fo::ScreenSetModel>(*compval);
        }
      }
    }
    return _screen_set.get();
  }


  void ScreenSet::lookup_ports() const {
    if (_port_names_read)
      return;

    if (auto ss = screen_set_model()) {
      _port_names_read = true;

      for (const auto& region : ss->_regions) {
        auto portnm = region._props.lookup_value_or("port", region._zone);

        if (portnm != k_main) {
          _port_names.emplace_back(portnm);
        }
        else if (portnm.empty()) {
          std::cout << "No port name found in props\n";
        }
      }
    }
  }


  //----------------------------------------------------------------------------

  bool is_property_be_inherited(const std::string& key) {
    using namespace std;

    const auto i_find =
      find_if(begin(s_properties), end(s_properties),
              [&key](const PropertyDefs& propinh) { return propinh._key == key; });
    return i_find != end(s_properties) ? i_find->_is_inherited : false;
  }


  ValueType::Kind property_default_type(const std::string& key) {
    using namespace std;

    const auto i_find =
      find_if(begin(s_properties), end(s_properties),
              [&key](const PropertyDefs& propinh) { return propinh._key == key; });
    return i_find != end(s_properties) ? i_find->_type : ValueType::k_bool;
  }


  //----------------------------------------------------------------------------

  using FoClassFactoryFunc =
    std::function<std::unique_ptr<IFormattingObject>(const PropertySpecs& props,
                                                     const Sosofo& sosofo)>;
  using FoClassFactoryPair = std::pair<std::string, FoClassFactoryFunc>;
  using FoClassFactoryMap = std::unordered_map<std::string, FoClassFactoryFunc>;


  template <typename FoClass, int N>
  struct FoClassFactory
  { static FoClassFactoryFunc make_factory_func(); };

  template <typename FoClass>
  struct FoClassFactory<FoClass, 0>
  {
    static FoClassFactoryFunc make_factory_func() {
      return
        [](const PropertySpecs&, const Sosofo&) { return estd::make_unique<FoClass>(); };
    }
  };

  template <typename FoClass>
  struct FoClassFactory<FoClass, 1>
  {
    static FoClassFactoryFunc make_factory_func() {
      return [](const PropertySpecs& p, const Sosofo&) {
        return estd::make_unique<FoClass>(p);
      };
    }
  };

  template <typename FoClass>
  struct FoClassFactory<FoClass, 2>
  {
    static FoClassFactoryFunc make_factory_func() {
      return [](const PropertySpecs& p, const Sosofo& s) {
        return estd::make_unique<FoClass>(p, s);
      };
    }
  };

  template <typename FoClass, int NumParam>
  FoClassFactoryPair make_fo_class_factory() {
    return std::make_pair(FoClass().classname(),
                          FoClassFactory<FoClass, NumParam>::make_factory_func());
  }

  std::unique_ptr<IFormattingObject> create_fo_by_classname(const std::string& classname,
                                                            const PropertySpecs& props,
                                                            const Sosofo& sosofo) {
    static auto s_fo_class_factory_map = FoClassFactoryMap{
      make_fo_class_factory<Anchor, 1>(),
      make_fo_class_factory<Box, 2>(),
      make_fo_class_factory<DisplayGroup, 2>(),
      make_fo_class_factory<FootNote, 2>(),
      make_fo_class_factory<LineField, 2>(),
      make_fo_class_factory<Link, 2>(),
      make_fo_class_factory<Literal, 1>(),
      make_fo_class_factory<ExternalGraphic, 1>(),
      make_fo_class_factory<PageNumber, 1>(),
      make_fo_class_factory<Paragraph, 2>(),
      make_fo_class_factory<ParagraphBreak, 0>(),
      make_fo_class_factory<Score, 2>(),
      make_fo_class_factory<ScreenSet, 2>(),
      make_fo_class_factory<ScrollSequence, 2>(),
      make_fo_class_factory<Sequence, 2>(),
      make_fo_class_factory<SimpleColumnSetSequence, 2>(),
      make_fo_class_factory<SimplePageSequence, 2>(),
    };

    const auto i_find = s_fo_class_factory_map.find(classname);
    if (i_find != s_fo_class_factory_map.end()) {
      return i_find->second(props, sosofo);
    }

    return nullptr;
  }


} // namespace fo
} // namespace eyestep
