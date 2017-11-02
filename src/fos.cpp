// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "fos.hpp"
#include "estd/memory.hpp"
#include "fo.hpp"
#include "sosofo.hpp"

#include <algorithm>
#include <limits>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>


namespace eyestep {

namespace fo {

  struct PropertyInherited
  {
    const std::string _key;
    const bool _is_inherited;
  };


  const auto s_property_inherited = std::vector<PropertyInherited>{
    {"box-type", true},                // Keyw: border, background, both
    {"display?", false},               // Bool
    {"box-corner-rounded?", false},    // Bool
    {"box-corner-radius", false},      // LengthSpec
    {"above?", true},                  // Bool
    {"background-color", false},       // Color
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
    {"column-number", false},          // Int
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


  const auto k_bottom = std::string("bottom");
  const auto k_left = std::string("left");
  const auto k_main = std::string("main");
  const auto k_right = std::string("right");
  const auto k_scroll = std::string("scroll");
  const auto k_text = std::string("text");
  const auto k_top = std::string("top");


  //----------------------------------------------------------------------------

  static Sosofo k_nil_sosofo;

  const std::vector<std::string>& Fo::ports() const {
    static const auto ports = std::vector<std::string>{};
    return ports;
  }


  const Sosofo& Fo::port(const std::string&) const {
    return k_nil_sosofo;
  }


  void Fo::set_port(const std::string&, const Sosofo&) {}


  //----------------------------------------------------------------------------

  Literal::Literal(const PropertySpecs& props)
    : Fo(props) {}


  /*! Returns the class name for this FOs @p class. */
  std::string Literal::classname() const {
    return "#literal";
  }


  /*! Return the set of defined properties */
  const PropertySpecs& Literal::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"text", ""}, {"language", ""},
    };

    return propspecs;
  }


  /*! Return a port by @p portName */
  const Sosofo& Literal::port(const std::string& portName) const {
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

  Paragraph::Paragraph(const PropertySpecs& props, const Sosofo& text_port)
    : Fo(props)
    , _text_port(text_port) {}


  std::string Paragraph::classname() const {
    return "#paragraph";
  }


  bool Paragraph::accepts_fo(const Sosofo& sosofo) const {
    using namespace std;

    return none_of(begin(sosofo), end(sosofo), [](const IFormattingObject& fo) {
      return dynamic_cast<const Paragraph*>(&fo) != nullptr;
    });
  }


  const PropertySpecs& Paragraph::default_properties() const {
    auto max_inf = std::numeric_limits<double>::infinity();

    static const auto propspecs = PropertySpecs{
      // clang-format off
      {"first-line-start-indent", LengthSpec(kInline, 0, k_em)},
      {"last-line-end-indent", LengthSpec(kInline, 1, k_em, 1, max_inf)},
      {"line-spacing", LengthSpec(kDimen, 14, k_pt)},
      {"font-caps", "normal"},
      {"font-name", "serif"},
      {"font-posture", "upright"},
      {"font-size", LengthSpec(kDimen, 10, k_pt)},
      {"font-weight", "medium"},
      {"language", ""},
      {"start-indent", LengthSpec(kInline, 0, k_em)},
      {"end-indent", LengthSpec(kInline, 0, k_em)},
      {"quadding", "justify"},
      {"space-before", LengthSpec(kDisplay, 0, k_pt)},
      {"space-after", LengthSpec(kDisplay, 0, k_pt)},
      {"keep-with-previous?", false},
      {"keep-with-next?", false},
      {"break-after?", false},
      {"break-before?", false},
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

  std::string ParagraphBreak::classname() const {
    return "#paragraph-break";
  }


  const PropertySpecs& ParagraphBreak::default_properties() const {
    static const auto propspecs = PropertySpecs{};
    return propspecs;
  }


  //----------------------------------------------------------------------------

  DisplayGroup::DisplayGroup(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props)
    , _text_port(sosofo) {}


  std::string DisplayGroup::classname() const {
    return "#display-group";
  }


  const PropertySpecs& DisplayGroup::default_properties() const {
    // clang-format off
    static const auto propspecs = PropertySpecs{
      {"space-before", LengthSpec(kDisplay, 0, k_pt)},
      {"space-after", LengthSpec(kDisplay, 0, k_pt)},
      {"break-before?", false},
      {"break-after?", false},
      {"font-caps", ""},
      {"font-name", ""},
      {"font-posture", ""},
      {"font-size", ""},
      {"font-weight", ""},
      {"lines", ""},
      {"color", ""},
      {"background-color", ""},
    };
    // clang-format on
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

  Box::Box(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props)
    , _text_port(sosofo) {}


  std::string Box::classname() const {
    return "#box";
  }


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
      {"break-before?", false},
      {"break-after?", false},
      {"line-thickness", LengthSpec(kDisplay, 1, k_pt)},
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

  Sequence::Sequence(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props)
    , _text_port(sosofo) {}


  std::string Sequence::classname() const {
    return "#sequence";
  }


  const PropertySpecs& Sequence::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"position-point-shift", LengthSpec(kDimen, 0, k_pt)},
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

  LineField::LineField(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props)
    , _text_port(sosofo) {}


  std::string LineField::classname() const {
    return "#line-field";
  }


  const PropertySpecs& LineField::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"field-width", LengthSpec(kInline, 0, k_pt)},
      {"field-align", "left"},
      {"inhibit-line-breaks?", false},
      {"position-point-shift", LengthSpec(kDimen, 0, k_pt)},
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

  Score::Score(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props)
    , _text_port(sosofo) {}


  std::string Score::classname() const {
    return "#score";
  }


  const PropertySpecs& Score::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"below?", false},
      {"above?", false},
      {"color", ""},
      {"line-thickness", LengthSpec(kDimen, 0, k_pt)},
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

  SimplePageSequence::SimplePageSequence(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props)
    , _text_port(sosofo) {}


  std::string SimplePageSequence::classname() const {
    return "#simple-page-sequence";
  }


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

  SimpleColumnSetSequence::SimpleColumnSetSequence(const PropertySpecs& props,
                                                   const Sosofo& text_port)
    : Fo(props)
    , _text_port(text_port) {}


  std::string SimpleColumnSetSequence::classname() const {
    return "#simple-column-set-sequence";
  }


  const PropertySpecs& SimpleColumnSetSequence::default_properties() const {
    static const auto propspecs = PropertySpecs{
      // clang-format off
      {"space-before", LengthSpec(kDisplay, 0, k_pt)},
      {"space-after", LengthSpec(kDisplay, 0, k_pt)},
      {"keep-with-previous?", false},
      {"keep-with-next?", false},
      {"break-after?", false},
      {"break-before?", false},
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

  ScrollSequence::ScrollSequence(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props)
    , _scroll_port(sosofo) {}


  std::string ScrollSequence::classname() const {
    return "#scroll-sequence";
  }


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

  PageNumber::PageNumber(const PropertySpecs& props)
    : Fo(props) {}


  std::string PageNumber::classname() const {
    return "#page-number";
  }


  const PropertySpecs& PageNumber::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"refid", "#current"},
    };
    return propspecs;
  }


  //----------------------------------------------------------------------------

  Anchor::Anchor(const PropertySpecs& props)
    : Fo(props) {}


  std::string Anchor::classname() const {
    return "#anchor";
  }


  const PropertySpecs& Anchor::default_properties() const {
    static const auto propspecs = PropertySpecs{
      {"id", false},
    };
    return propspecs;
  }


  //----------------------------------------------------------------------------

  FootNote::FootNote(const PropertySpecs& props, const Sosofo& sosofo)
    : Fo(props)
    , _text_port(sosofo) {}


  std::string FootNote::classname() const {
    return "#foot-note";
  }


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
    , _ports({{k_main, sosofo}}) {
    if (auto compval =
          props.lookup_value<std::shared_ptr<fo::ICompoundValue>>("screen-set-model")) {
      if (auto screen_set_model =
            std::dynamic_pointer_cast<fo::ScreenSetModel>(*compval)) {
        for (const auto& region : screen_set_model->_regions) {
          auto portnm = region._props.lookup_value_or("port", region._zone);

          if (portnm != k_main) {
            _ports[portnm] = Sosofo{};
            _port_names.emplace_back(portnm);
          }
        }
      }
    }
  }


  std::string ScreenSet::classname() const {
    return "#screen-set";
  }


  const PropertySpecs& ScreenSet::default_properties() const {
    static const auto propspecs = PropertySpecs{{"title", false},
                                                {"screen-set-model", false},
                                                {"html.add-css-link", false}};

    return propspecs;
  }


  const std::vector<std::string>& ScreenSet::ports() const {
    return _port_names;
  }


  const Sosofo& ScreenSet::port(const std::string& portnm) const {
    using namespace std;

    auto i_port = _ports.find(portnm);
    if (i_port != end(_ports))
      return i_port->second;

    return k_nil_sosofo;
  }


  void ScreenSet::set_port(const std::string& portnm, const Sosofo& sosofo) {
    using namespace std;

    if (portnm != k_main) {
      auto i_portnm = find(begin(_port_names), end(_port_names), portnm);
      if (i_portnm != end(_port_names)) {
        _ports[portnm] = sosofo;
      }
    }
  }


  //----------------------------------------------------------------------------

  bool is_property_be_inherited(const std::string& key) {
    using namespace std;

    const auto i_find =
      find_if(begin(s_property_inherited), end(s_property_inherited),
              [&key](const PropertyInherited& propinh) { return propinh._key == key; });
    return i_find != end(s_property_inherited) ? i_find->_is_inherited : false;
  }


  //----------------------------------------------------------------------------

  using FoClassFactoryFunc =
    std::function<std::unique_ptr<IFormattingObject>(const PropertySpecs& props,
                                                     const Sosofo& sosofo)>;

  using FoClassFactoryMap = std::unordered_map<std::string, FoClassFactoryFunc>;

  FoClassFactoryMap s_fo_class_factory_map;


  template <typename FoClass, typename FactoryFunc>
  void register_fo_class_factory(FactoryFunc factory_func) {
    FoClass fo_class;
    const auto classname = fo_class.classname();

    const auto i_find = s_fo_class_factory_map.find(classname);
    if (i_find == s_fo_class_factory_map.end()) {
      s_fo_class_factory_map[classname] = factory_func;
    }
  }


  template <typename FoClass>
  void register_fo_class_factory() {
    register_fo_class_factory<FoClass>([](const PropertySpecs& p, const Sosofo& s) {
      return ::estd::make_unique<FoClass>(p, s);
    });
  }


  template <typename FoClass>
  void register_fo_class_factory_props() {
    register_fo_class_factory<FoClass>([](const PropertySpecs& p, const Sosofo&) {
      return ::estd::make_unique<FoClass>(p);
    });
  }


  std::unique_ptr<IFormattingObject> create_fo_by_classname(const std::string& classname,
                                                            const PropertySpecs& props,
                                                            const Sosofo& sosofo) {
    if (s_fo_class_factory_map.empty()) {
      register_fo_class_factory_props<Literal>();

      register_fo_class_factory<ParagraphBreak>([](const PropertySpecs&, const Sosofo&) {
        return ::estd::make_unique<ParagraphBreak>();
      });

      register_fo_class_factory<Paragraph>();
      register_fo_class_factory<DisplayGroup>();
      register_fo_class_factory<Box>();
      register_fo_class_factory<Sequence>();
      register_fo_class_factory<LineField>();
      register_fo_class_factory<Score>();
      register_fo_class_factory<SimplePageSequence>();
      register_fo_class_factory<SimpleColumnSetSequence>();
      register_fo_class_factory<ScrollSequence>();
      register_fo_class_factory<ScreenSet>();
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


  std::ostream& operator<<(std::ostream& os, const LengthSpec& ls) {
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


  std::ostream& operator<<(std::ostream& os, const Color& co) {
    os << "<color:";

    switch (co._space) {
    case kRGB:
      os << "rgb:" << co._rgb._red << "," << co._rgb._green << "," << co._rgb._blue;
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
