// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"

#include <boost/variant/get.hpp>

#include <string>
#include <vector>
#include <limits>
#include <ostream>
#include <iostream>
#include <unordered_map>


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
    {"end-indent", true},              // Dimen
    {"end-margin", false},             // Dimen
    {"field-width", false},            // Dimen
    {"field-align", false},            // Keyw: left, right, center
    {"first-line-start-indent", true}, // Dimen
    {"last-line-end-indent", true},    // Dimen
    {"font-name", true},               // String
    {"font-posture", true},            // Keyw: upright, italic, oblique
    {"font-size", true},               // Dimen
    {"font-weight", true},             // Keyw: medium, bold, semibold,
    {"font-caps", true},               // Keyw: normal, caps, smallcaps,
    {"inhibit-line-breaks?", false},   // Bool
    {"language", true},                // String
    {"line-spacing", true},            // Dimen
    {"line-thickness", true},          // Dimen
    {"quadding", true},                // Keyw: left, right, center, justify
    {"space-after", false},            // Dimen
    {"space-before", false},           // Dimen
    {"start-indent", false},           // Dimen
    {"start-margin", false},           // Dimen
    {"title", false},                  // Sosofo
    {"keep-with-previous?", false},    // Bool
    {"keep-with-next?", false},        // Bool
    {"lines", true},                   // Keyw: wrap, asis, asis-wrap, none
    {"numbered-lines?", true},         // Bool
    {"line-number-side", true},        // Keyw: start, end, inside, outside
    {"asis-wrap-indent", true},        // Dimen
    {"whitespace-treatment", true},    // Keyw: preserve, collapse, ignore
    {"page-width", false},             // Dimen
    {"page-height", false},            // Dimen
    {"left-margin", false},            // Dimen
    {"right-margin", false},           // Dimen
    {"top-margin", false},             // Dimen
    {"bottom-margin", false},          // Dimen
    {"header-margin", false},          // Dimen
    {"footer-margin", false},          // Dimen
    {"left-header", false},            // Sosofo
    {"center-header", false},          // Sosofo
    {"right-header", false},           // Sosofo
    {"left-footer", false},            // Sosofo
    {"center-footer", false},          // Sosofo
    {"right-footer", false},           // Sosofo
    {"position-point-shift", false},   // Dimen
    {"text", false},                   // String
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
    const auto i_find =
      std::find_if(_props.begin(), _props.end(), [](const PropertySpec& spec) {
        return spec._name == "text";
      });
    if (i_find != _props.end()) {
      if (const std::string* val =
            boost::get<const std::string>(&i_find->_value)) {
        return *val;
      }
    }

    return "";
  }


  //----------------------------------------------------------------------------

  Paragraph::Paragraph(const PropertySpecs& props, const Sosofo& textPort)
    : Fo(props), _text_port(textPort)
  {
  }

  std::string Paragraph::classname() const { return "#paragraph"; }

  const PropertySpecs& Paragraph::default_properties() const
  {
    double max_inf = std::numeric_limits<double>::infinity();

    static PropertySpecs propspecs = {
      PropertySpec("first-line-start-indent", Dimen(0, k_em)),
      PropertySpec("last-line-end-indent", Dimen(1, k_em, 1, max_inf)),
      PropertySpec("line-spacing", Dimen(14, k_pt)),
      PropertySpec("font-caps", "normal"), PropertySpec("font-name", "serif"),
      PropertySpec("font-posture", "upright"),
      PropertySpec("font-size", Dimen(10, k_pt)),
      PropertySpec("font-weight", "medium"), PropertySpec("language", ""),
      PropertySpec("start-indent", Dimen(0, k_em)),
      PropertySpec("end-indent", Dimen(0, k_em)),
      PropertySpec("quadding", "justify"),
      PropertySpec("space-before", Dimen(0, k_pt)),
      PropertySpec("space-after", Dimen(0, k_pt)),
      PropertySpec("keep-with-previous?", false),
      PropertySpec("keep-with-next?", false),
      PropertySpec("break-after?", false), PropertySpec("break-before?", false),
      PropertySpec("lines", "wrap"),
      PropertySpec("whitespace-treatment", "collapse"),
      PropertySpec("asis-wrap-indent", 10),
      PropertySpec("numbered-lines?", false),
      PropertySpec("line-number-side", "start"),
      PropertySpec("position-point-shift", Dimen(0, k_pt)),
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
    static PropertySpecs propspecs = {
      PropertySpec("space-before", Dimen(0, k_pt)),
      PropertySpec("space-after", Dimen(0, k_pt)),
      PropertySpec("break-before?", false), PropertySpec("break-after?", false),
      PropertySpec("font-caps", ""), PropertySpec("font-name", ""),
      PropertySpec("font-posture", ""), PropertySpec("font-size", ""),
      PropertySpec("font-weight", ""), PropertySpec("lines", ""),
    };
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
      PropertySpec("position-point-shift", Dimen(0, k_pt)),
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
      PropertySpec("field-width", Dimen(0, k_pt)),
      PropertySpec("field-align", "left"),
      PropertySpec("inhibit-line-breaks?", false),
      PropertySpec("position-point-shift", Dimen(0, k_pt)),
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
      PropertySpec("color", ""), PropertySpec("line-thickness", Dimen(0, k_pt)),
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
      {PropertySpec("font-caps", "normal"), PropertySpec("font-name", "serif"),
       PropertySpec("font-posture", "upright"),
       PropertySpec("font-size", Dimen(10, k_pt)),
       PropertySpec("font-weight", "medium"), PropertySpec("lines", "wrap"),
       PropertySpec("whitespace-treatment", "collapse"),
       PropertySpec("start-margin", Dimen(0, k_pt)),
       PropertySpec("end-margin", Dimen(0, k_pt)),
       PropertySpec("page-width", Dimen(210, k_mm)),
       PropertySpec("page-height", Dimen(297, k_mm)),
       PropertySpec("left-margin", Dimen(30, k_mm)),
       PropertySpec("right-margin", Dimen(30, k_mm)),
       PropertySpec("top-margin", Dimen(20, k_mm)),
       PropertySpec("bottom-margin", Dimen(30, k_mm)),
       PropertySpec("header-margin", Dimen(10, k_mm)),
       PropertySpec("footer-margin", Dimen(20, k_mm)),
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

  ScrollSequence::ScrollSequence(const PropertySpecs& props,
                                 const Sosofo& sosofo)
    : Fo(props), _scroll_port(sosofo)
  {
  }

  std::string ScrollSequence::classname() const { return "#scroll-sequence"; }

  const PropertySpecs& ScrollSequence::default_properties() const
  {
    static PropertySpecs propspecs = {
      PropertySpec("font-caps", "normal"), PropertySpec("font-name", "serif"),
      PropertySpec("font-posture", "upright"),
      PropertySpec("font-size", Dimen(10, k_pt)),
      PropertySpec("font-weight", "medium"), PropertySpec("title", false),
      PropertySpec("start-margin", Dimen(0, k_pt)),
      PropertySpec("end-margin", Dimen(0, k_pt)),
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
        return estd::make_unique<FoClass>(p, s);
      });
  }

  template <typename FoClass>
  void register_fo_class_factory_props()
  {
    register_fo_class_factory<FoClass>(
      [](const PropertySpecs& p, const Sosofo&) {
        return estd::make_unique<FoClass>(p);
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
          return estd::make_unique<ParagraphBreak>();
        });

      register_fo_class_factory<Paragraph>();
      register_fo_class_factory<DisplayGroup>();
      register_fo_class_factory<Sequence>();
      register_fo_class_factory<LineField>();
      register_fo_class_factory<Score>();
      register_fo_class_factory<SimplePageSequence>();
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


  std::ostream& operator<<(std::ostream& os, const Dimen& dimen)
  {
    auto unit_name = [](Unit un) {
      switch (un) {
      case k_pt:
        return "pt";
      case k_m:
        return "m";
      case k_mm:
        return "mm";
      case k_cm:
        return "cm";
      case k_em:
        return "em";
      }
    };

    os << "<dimen:" << dimen._value << unit_name(dimen._unit);
    if (dimen._value != dimen._min) {
      os << " min " << dimen._min << unit_name(dimen._unit);
    }
    if (dimen._max == std::numeric_limits<double>::infinity()) {
      os << " plus INF";
    }
    else if (dimen._value != dimen._max) {
      os << " plus " << dimen._max << unit_name(dimen._unit);
    }

    os << ">";

    return os;
  }

} // ns fo
} // ns eyestep
