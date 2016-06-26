// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "fos.hpp"
#include "estd/memory.hpp"
#include "fo.hpp"
#include "sosofo.hpp"

#include <iostream>
#include <limits>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>


namespace eyestep {

namespace fo {

  struct PropertyInherited {
    const std::string _key;
    const bool _is_inherited;
  };

  const auto s_property_inherited = std::vector<PropertyInherited>{
    {"above?", true},                  // Bool
    {"background-color", true},        // Color
    {"background-tile", false},        // String: path to ext. graphics
    {"below?", true},                  // Bool
    {"break-after?", false},           // Bool
    {"break-before?", false},          // Bool
    {"class", false},                  // String
    {"color", true},                   // Color
    {"end-indent", true},              // LengthSpec
    {"end-margin", false},             // LengthSpec
    {"field-width", false},            // LengthSpec
    {"field-align", false},            // Keyw: left, right, center
    {"first-line-start-indent", true}, // LengthSpec
    {"gutter-width", false},           // LengthSpec
    {"last-line-end-indent", true},    // LengthSpec
    {"font-name", true},               // String
    {"font-posture", true},            // Keyw: upright, italic, oblique
    {"font-size", true},               // LengthSpec
    {"font-weight", true},             // Keyw: medium, bold, semibold,
    {"font-caps", true},               // Keyw: normal, caps, smallcaps,
    {"inhibit-line-breaks?", false},   // Bool
    {"language", true},                // String
    {"line-spacing", true},            // LengthSpec
    {"line-thickness", true},          // LengthSpec
    {"quadding", true},                // Keyw: left, right, center, justify
    {"space-after", false},            // LengthSpec
    {"space-before", false},           // LengthSpec
    {"start-indent", false},           // LengthSpec
    {"start-margin", false},           // LengthSpec
    {"title", false},                  // Sosofo
    {"keep-with-previous?", false},    // Bool
    {"keep-with-next?", false},        // Bool
    {"lines", true},                   // Keyw: wrap, asis, asis-wrap, none
    {"numbered-lines?", true},         // Bool
    {"line-number-side", true},        // Keyw: start, end, inside, outside
    {"asis-wrap-indent", true},        // LengthSpec
    {"whitespace-treatment", true},    // Keyw: preserve, collapse, ignore
    {"page-width", false},             // LengthSpec
    {"page-height", false},            // LengthSpec
    {"left-margin", false},            // LengthSpec
    {"right-margin", false},           // LengthSpec
    {"top-margin", false},             // LengthSpec
    {"bottom-margin", false},          // LengthSpec
    {"header-margin", false},          // LengthSpec
    {"footer-margin", false},          // LengthSpec
    {"left-header", false},            // Sosofo
    {"center-header", false},          // Sosofo
    {"right-header", false},           // Sosofo
    {"left-footer", false},            // Sosofo
    {"center-footer", false},          // Sosofo
    {"right-footer", false},           // Sosofo
    {"position-point-shift", false},   // LengthSpec
    {"text", false},                   // String
    {"width", false},                  // LengthSpec
  };


  //----------------------------------------------------------------------------

  static Sosofo k_nil_sosofo;

  const std::vector<std::string>& Fo::ports() const
  {
    static const auto ports = std::vector<std::string>{};
    return ports;
  }


  const Sosofo& Fo::port(const std::string&) const { return k_nil_sosofo; }


  //----------------------------------------------------------------------------

  Literal::Literal(const PropertySpecs& props) : Fo(props) {}

  /*! Returns the class name for this FOs @p class. */
  std::string Literal::classname() const { return "#literal"; }

  /*! Return the set of defined properties */
  const PropertySpecs& Literal::default_properties() const
  {
    static const PropertySpecs propspecs = {
      PropertySpec("text", ""), PropertySpec("language", ""),
    };

    return propspecs;
  }

  /*! Return a port by @p portName */
  const Sosofo& Literal::port(const std::string& portName) const
  {
    return k_nil_sosofo;
  }

  std::string Literal::text() const
  {
    if (auto spec = _props.lookup_key("text")) {
      if (const std::string* val = fo::get<const std::string>(&spec->_value)) {
        return *val;
      }
    }

    return "";
  }


  //----------------------------------------------------------------------------

  Paragraph::Paragraph(const PropertySpecs& props, const Sosofo& text_port)
    : Fo(props), _text_port(text_port)
  {
  }

  std::string Paragraph::classname() const { return "#paragraph"; }

  bool Paragraph::accepts_fo(const Sosofo& fo) const
  {
    for (int i = 0; i < fo.length(); i++) {
      if (dynamic_cast<const Paragraph*>(fo[i]) != nullptr)
        return false;
    }
    return true;
  }

  const PropertySpecs& Paragraph::default_properties() const
  {
    double max_inf = std::numeric_limits<double>::infinity();

    static PropertySpecs propspecs = {
      // clang-format off
      PropertySpec("first-line-start-indent", LengthSpec(kInline, 0, k_em)),
      PropertySpec("last-line-end-indent", LengthSpec(kInline, 1, k_em, 1, max_inf)),
      PropertySpec("line-spacing", LengthSpec(kDimen, 14, k_pt)),
      PropertySpec("font-caps", "normal"),
      PropertySpec("font-name", "serif"),
      PropertySpec("font-posture", "upright"),
      PropertySpec("font-size", LengthSpec(kDimen, 10, k_pt)),
      PropertySpec("font-weight", "medium"),
      PropertySpec("language", ""),
      PropertySpec("start-indent", LengthSpec(kInline, 0, k_em)),
      PropertySpec("end-indent", LengthSpec(kInline, 0, k_em)),
      PropertySpec("quadding", "justify"),
      PropertySpec("space-before", LengthSpec(kDisplay, 0, k_pt)),
      PropertySpec("space-after", LengthSpec(kDisplay, 0, k_pt)),
      PropertySpec("keep-with-previous?", false),
      PropertySpec("keep-with-next?", false),
      PropertySpec("break-after?", false),
      PropertySpec("break-before?", false),
      PropertySpec("lines", "wrap"),
      PropertySpec("whitespace-treatment", "collapse"),
      PropertySpec("asis-wrap-indent", 10),
      PropertySpec("numbered-lines?", false),
      PropertySpec("line-number-side", "start"),
      PropertySpec("position-point-shift", LengthSpec(kDimen, 0, k_pt)),
      PropertySpec("color", ""),
      PropertySpec("background-color", ""),
      // clang-format on
    };
    return propspecs;
  }

  const std::vector<std::string>& Paragraph::ports() const
  {
    static const auto ports = std::vector<std::string>{
      "text",
    };
    return ports;
  }

  const Sosofo& Paragraph::port(const std::string& portname) const
  {
    if (portname == "text") {
      return _text_port;
    }

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  std::string ParagraphBreak::classname() const { return "#paragraph-break"; }

  const PropertySpecs& ParagraphBreak::default_properties() const
  {
    static PropertySpecs propspecs = {};
    return propspecs;
  }


  //----------------------------------------------------------------------------

  DisplayGroup::DisplayGroup(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props), _text_port(sosofo)
  {
  }

  std::string DisplayGroup::classname() const { return "#display-group"; }

  const PropertySpecs& DisplayGroup::default_properties() const
  {
    // clang-format off
    static PropertySpecs propspecs = {
      PropertySpec("space-before", LengthSpec(kDisplay, 0, k_pt)),
      PropertySpec("space-after", LengthSpec(kDisplay, 0, k_pt)),
      PropertySpec("break-before?", false),
      PropertySpec("break-after?", false),
      PropertySpec("font-caps", ""),
      PropertySpec("font-name", ""),
      PropertySpec("font-posture", ""),
      PropertySpec("font-size", ""),
      PropertySpec("font-weight", ""),
      PropertySpec("lines", ""),
      PropertySpec("color", ""),
      PropertySpec("background-color", ""),
    };
    // clang-format on
    return propspecs;
  }

  const std::vector<std::string>& DisplayGroup::ports() const
  {
    static const auto ports = std::vector<std::string>{
      "text",
    };
    return ports;
  }

  const Sosofo& DisplayGroup::port(const std::string& portname) const
  {
    if (portname == "text") {
      return _text_port;
    }

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  Sequence::Sequence(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props), _text_port(sosofo)
  {
  }

  std::string Sequence::classname() const { return "#sequence"; }

  const PropertySpecs& Sequence::default_properties() const
  {
    static PropertySpecs propspecs = {
      PropertySpec("position-point-shift", LengthSpec(kDimen, 0, k_pt)),
    };
    return propspecs;
  }

  const std::vector<std::string>& Sequence::ports() const
  {
    static const auto ports = std::vector<std::string>{
      "text",
    };
    return ports;
  }

  const Sosofo& Sequence::port(const std::string& portname) const
  {
    if (portname == "text") {
      return _text_port;
    }

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  LineField::LineField(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props), _text_port(sosofo)
  {
  }

  std::string LineField::classname() const { return "#line-field"; }

  const PropertySpecs& LineField::default_properties() const
  {
    static PropertySpecs propspecs = {
      PropertySpec("field-width", LengthSpec(kInline, 0, k_pt)),
      PropertySpec("field-align", "left"),
      PropertySpec("inhibit-line-breaks?", false),
      PropertySpec("position-point-shift", LengthSpec(kDimen, 0, k_pt)),
    };
    return propspecs;
  }

  const std::vector<std::string>& LineField::ports() const
  {
    static const auto ports = std::vector<std::string>{
      "text",
    };
    return ports;
  }

  const Sosofo& LineField::port(const std::string& portname) const
  {
    if (portname == "text") {
      return _text_port;
    }

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  Score::Score(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props), _text_port(sosofo)
  {
  }

  std::string Score::classname() const { return "#score"; }

  const PropertySpecs& Score::default_properties() const
  {
    static PropertySpecs propspecs = {
      PropertySpec("below?", false), PropertySpec("above?", false),
      PropertySpec("color", ""),
      PropertySpec("line-thickness", LengthSpec(kDimen, 0, k_pt)),
    };
    return propspecs;
  }

  const std::vector<std::string>& Score::ports() const
  {
    static const auto ports = std::vector<std::string>{
      "text",
    };
    return ports;
  }

  const Sosofo& Score::port(const std::string& portname) const
  {
    if (portname == "text") {
      return _text_port;
    }

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  SimplePageSequence::SimplePageSequence(const PropertySpecs& props,
                                         const Sosofo& sosofo)
    : Fo(props), _text_port(sosofo)
  {
  }

  std::string SimplePageSequence::classname() const
  {
    return "#simple-page-sequence";
  }

  const PropertySpecs& SimplePageSequence::default_properties() const
  {
    static PropertySpecs propspecs =
      {PropertySpec("font-caps", "normal"),
       PropertySpec("font-name", "serif"),
       PropertySpec("font-posture", "upright"),
       PropertySpec("font-size", LengthSpec(kDimen, 10, k_pt)),
       PropertySpec("font-weight", "medium"),
       PropertySpec("lines", "wrap"),
       PropertySpec("whitespace-treatment", "collapse"),
       PropertySpec("start-margin", LengthSpec(kInline, 0, k_pt)),
       PropertySpec("end-margin", LengthSpec(kInline, 0, k_pt)),
       PropertySpec("page-width", LengthSpec(kDimen, 210, k_mm)),
       PropertySpec("page-height", LengthSpec(kDimen, 297, k_mm)),
       PropertySpec("left-margin", LengthSpec(kInline, 30, k_mm)),
       PropertySpec("right-margin", LengthSpec(kDimen, 30, k_mm)),
       PropertySpec("top-margin", LengthSpec(kDimen, 20, k_mm)),
       PropertySpec("bottom-margin", LengthSpec(kDimen, 30, k_mm)),
       PropertySpec("header-margin", LengthSpec(kDimen, 10, k_mm)),
       PropertySpec("footer-margin", LengthSpec(kDimen, 20, k_mm)),
       PropertySpec("left-header", std::make_shared<Sosofo>()),
       PropertySpec("center-header", std::make_shared<Sosofo>()),
       PropertySpec("right-header", std::make_shared<Sosofo>()),
       PropertySpec("left-footer", std::make_shared<Sosofo>()),
       PropertySpec("center-footer", std::make_shared<Sosofo>()),
       PropertySpec("right-footer", std::make_shared<Sosofo>())};
    return propspecs;
  }

  const std::vector<std::string>& SimplePageSequence::ports() const
  {
    static const auto ports = std::vector<std::string>{
      "text",
    };
    return ports;
  }

  const Sosofo& SimplePageSequence::port(const std::string& portname) const
  {
    if (portname == "text") {
      return _text_port;
    }

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  SimpleColumnSetSequence::SimpleColumnSetSequence(const PropertySpecs& props,
                                                   const Sosofo& text_port)
    : Fo(props), _text_port(text_port)
  {
  }

  std::string SimpleColumnSetSequence::classname() const
  {
    return "#simple-column-set-sequence";
  }

  const PropertySpecs& SimpleColumnSetSequence::default_properties() const
  {
    static PropertySpecs propspecs = {
      // clang-format off
      PropertySpec("space-before", LengthSpec(kDisplay, 0, k_pt)),
      PropertySpec("space-after", LengthSpec(kDisplay, 0, k_pt)),
      PropertySpec("keep-with-previous?", false),
      PropertySpec("keep-with-next?", false),
      PropertySpec("break-after?", false),
      PropertySpec("break-before?", false),
      PropertySpec("column-number", 1),
      PropertySpec("gutter-width", LengthSpec(kDimen, 21, k_pt)),
      // clang-format on
    };
    return propspecs;
  }

  const std::vector<std::string>& SimpleColumnSetSequence::ports() const
  {
    static const auto ports = std::vector<std::string>{
      "text",
    };
    return ports;
  }

  const Sosofo& SimpleColumnSetSequence::port(const std::string& portname) const
  {
    if (portname == "text") {
      return _text_port;
    }

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  ScrollSequence::ScrollSequence(const PropertySpecs& props,
                                 const Sosofo& sosofo)
    : Fo(props), _scroll_port(sosofo)
  {
  }

  std::string ScrollSequence::classname() const { return "#scroll-sequence"; }

  const PropertySpecs& ScrollSequence::default_properties() const
  {
    static PropertySpecs propspecs = {
      PropertySpec("font-caps", "normal"),
      PropertySpec("font-name", "serif"),
      PropertySpec("font-posture", "upright"),
      PropertySpec("font-size", LengthSpec(kDimen, 10, k_pt)),
      PropertySpec("font-weight", "medium"),
      PropertySpec("title", false),
      PropertySpec("width", LengthSpec(kDimen, 600, k_px)),
      PropertySpec("start-margin", LengthSpec(kDimen, 0, k_pt)),
      PropertySpec("end-margin", LengthSpec(kDimen, 0, k_pt)),
      PropertySpec("background-color", false),
      PropertySpec("background-tile", false),
    };
    return propspecs;
  }

  const std::vector<std::string>& ScrollSequence::ports() const
  {
    static const auto ports = std::vector<std::string>{
      "scroll",
    };
    return ports;
  }

  const Sosofo& ScrollSequence::port(const std::string& portname) const
  {
    if (portname == "scroll") {
      return _scroll_port;
    }

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  PageNumber::PageNumber(const PropertySpecs& props) : Fo(props) {}

  std::string PageNumber::classname() const { return "#page-number"; }

  const PropertySpecs& PageNumber::default_properties() const
  {
    static PropertySpecs propspecs = {
      PropertySpec("refid", "#current"),
    };
    return propspecs;
  }


  //----------------------------------------------------------------------------

  Anchor::Anchor(const PropertySpecs& props) : Fo(props) {}

  std::string Anchor::classname() const { return "#anchor"; }

  const PropertySpecs& Anchor::default_properties() const
  {
    static PropertySpecs propspecs = {
      PropertySpec("id", false),
    };
    return propspecs;
  }


  //----------------------------------------------------------------------------

  FootNote::FootNote(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props), _text_port(sosofo)
  {
  }

  std::string FootNote::classname() const { return "#foot-note"; }

  const PropertySpecs& FootNote::default_properties() const
  {
    static PropertySpecs propspecs = {
      PropertySpec("id", false),
    };
    return propspecs;
  }

  const std::vector<std::string>& FootNote::ports() const
  {
    static const auto ports = std::vector<std::string>{
      "text",
    };
    return ports;
  }

  const Sosofo& FootNote::port(const std::string& portname) const
  {
    if (portname == "text") {
      return _text_port;
    }

    return k_nil_sosofo;
  }


  //----------------------------------------------------------------------------

  bool is_property_be_inherited(const std::string& key)
  {
    const auto i_find =
      std::find_if(s_property_inherited.begin(), s_property_inherited.end(),
                   [&key](const PropertyInherited& propinh) {
                     return propinh._key == key;
                   });
    return i_find != s_property_inherited.end() ? i_find->_is_inherited : false;
  }


  //----------------------------------------------------------------------------

  using FoClassFactoryFunc =
    std::function<std::unique_ptr<IFormattingObject>(const PropertySpecs& props,
                                                     const Sosofo& sosofo)>;

  using FoClassFactoryMap = std::unordered_map<std::string, FoClassFactoryFunc>;

  FoClassFactoryMap s_fo_class_factory_map;

  template <typename FoClass, typename FactoryFunc>
  void register_fo_class_factory(FactoryFunc factory_func)
  {
    FoClass fo_class;
    const auto classname = fo_class.classname();

    const auto i_find = s_fo_class_factory_map.find(classname);
    if (i_find == s_fo_class_factory_map.end()) {
      s_fo_class_factory_map[classname] = factory_func;
    }
  }

  template <typename FoClass>
  void register_fo_class_factory()
  {
    register_fo_class_factory<FoClass>(
      [](const PropertySpecs& p, const Sosofo& s) {
        return ::estd::make_unique<FoClass>(p, s);
      });
  }

  template <typename FoClass>
  void register_fo_class_factory_props()
  {
    register_fo_class_factory<FoClass>(
      [](const PropertySpecs& p, const Sosofo&) {
        return ::estd::make_unique<FoClass>(p);
      });
  }


  std::unique_ptr<IFormattingObject>
  create_fo_by_classname(const std::string& classname,
                         const PropertySpecs& props, const Sosofo& sosofo)
  {
    if (s_fo_class_factory_map.empty()) {
      register_fo_class_factory_props<Literal>();

      register_fo_class_factory<ParagraphBreak>(
        [](const PropertySpecs&, const Sosofo&) {
          return ::estd::make_unique<ParagraphBreak>();
        });

      register_fo_class_factory<Paragraph>();
      register_fo_class_factory<DisplayGroup>();
      register_fo_class_factory<Sequence>();
      register_fo_class_factory<LineField>();
      register_fo_class_factory<Score>();
      register_fo_class_factory<SimplePageSequence>();
      register_fo_class_factory<SimpleColumnSetSequence>();
      register_fo_class_factory<ScrollSequence>();
      register_fo_class_factory<FootNote>();

      register_fo_class_factory_props<PageNumber>();
      register_fo_class_factory_props<Anchor>();
    }

    const auto i_find = s_fo_class_factory_map.find(classname);
    if (i_find != s_fo_class_factory_map.end()) {
      return i_find->second(props, sosofo);
    }

    return nullptr;
  }


  std::ostream& operator<<(std::ostream& os, const LengthSpec& ls)
  {
    auto unit_name = [](Unit un) {
      switch (un) {
      case k_pt:
        return "pt";
      case k_px:
        return "px";
      case k_m:
        return "m";
      case k_mm:
        return "mm";
      case k_cm:
        return "cm";
      case k_em:
        return "em";
      case k_in:
        return "in";
      }
    };

    os << "<length-spec:" << ls._value << unit_name(ls._unit);
    if (ls._value != ls._min) {
      os << " min " << ls._min << unit_name(ls._unit);
    }
    if (ls._max == std::numeric_limits<double>::infinity()) {
      os << " plus INF";
    }
    else if (ls._value != ls._max) {
      os << " plus " << ls._max << unit_name(ls._unit);
    }

    os << ">";

    return os;
  }


  std::ostream& operator<<(std::ostream& os, const Color& co)
  {
    os << "<color:";

    switch (co._space) {
    case kRGB:
      os << "rgb:" << co._rgb._red << "," << co._rgb._green << ","
         << co._rgb._blue;
      break;
    case kCMYK:
      os << "cmyk" << co._cmyk._cyan << "," << co._cmyk._magenta << ","
         << co._cmyk._yellow << "," << co._cmyk._black;
      break;
    case kGray:
      os << "gray:" << co._gray;
      break;
    }

    os << ">";
    return os;
  }

} // ns fo
} // ns eyestep
