// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"

#include <string>
#include <vector>
#include <limits>
#include <ostream>


namespace eyestep {

namespace fo {

  struct PropertyInherited {
    const std::string mKey;
    const bool mIsInherited;
  };

  const auto propertyInherited = std::vector<PropertyInherited>{
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
      {"start-indent", true},            // Dimen
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
  };


  //----------------------------------------------------------------------------

  static Sosofo kNilSosofo;

  const std::vector<std::string>& Fo::ports() const
  {
    static const auto ports = std::vector<std::string>{};
    return ports;
  }


  //----------------------------------------------------------------------------

  Literal::Literal(std::string data) : mData(data) {}

  /*! Returns the class name for this FOs @p class. */
  std::string Literal::className() const { return "#literal"; }

  /*! Return the set of defined properties */
  const PropertySpecs& Literal::defaultProperties() const
  {
    static const PropertySpecs propspecs;

    return propspecs;
  }

  /*! Return a port by @p portName */
  const Sosofo& Literal::port(const std::string& portName) const
  {
    return kNilSosofo;
  }


  //----------------------------------------------------------------------------

  Paragraph::Paragraph(const PropertySpecs& props, const Sosofo& textPort)
      : Fo(props), mTextPort(textPort)
  {
  }

  std::string Paragraph::className() const { return "#paragraph"; }

  const PropertySpecs& Paragraph::defaultProperties() const
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
        PropertySpec("break-after?", false),
        PropertySpec("break-before?", false), PropertySpec("lines", "wrap"),
        PropertySpec("whitespace-treatment", "collapse"),
        PropertySpec("asis-wrap-indent", 10),
        PropertySpec("numbered-lines?", false),
        PropertySpec("line-number-side", "start"),
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

  const Sosofo& Paragraph::port(const std::string& portName) const
  {
    if (portName == "text") {
      return mTextPort;
    }

    return kNilSosofo;
  }


  //----------------------------------------------------------------------------

  std::string ParagraphBreak::className() const { return "#paragraph-break"; }

  const PropertySpecs& ParagraphBreak::defaultProperties() const
  {
    static PropertySpecs propspecs = {};
    return propspecs;
  }

  const Sosofo& ParagraphBreak::port(const std::string& portName) const
  {
    return kNilSosofo;
  }


  //----------------------------------------------------------------------------

  DisplayGroup::DisplayGroup(const PropertySpecs& props, const Sosofo& sosofo)
      : Fo(props), mTextPort(sosofo)
  {
  }

  std::string DisplayGroup::className() const { return "#display-group"; }

  const PropertySpecs& DisplayGroup::defaultProperties() const
  {
    static PropertySpecs propspecs = {
        PropertySpec("space-before", Dimen(0, k_pt)),
        PropertySpec("space-after", Dimen(0, k_pt)),
        PropertySpec("break-before?", false),
        PropertySpec("break-after?", false), PropertySpec("font-caps", ""),
        PropertySpec("font-name", ""), PropertySpec("font-posture", ""),
        PropertySpec("font-size", ""), PropertySpec("font-weight", ""),
        PropertySpec("lines", ""),
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

  const Sosofo& DisplayGroup::port(const std::string& portName) const
  {
    if (portName == "text") {
      return mTextPort;
    }

    return kNilSosofo;
  }


  //----------------------------------------------------------------------------

  SimplePageSequence::SimplePageSequence(const PropertySpecs& props,
                                         const Sosofo& sosofo)
      : Fo(props), mTextPort(sosofo)
  {
  }

  std::string SimplePageSequence::className() const
  {
    return "#simple-page-sequence";
  }

  const PropertySpecs& SimplePageSequence::defaultProperties() const
  {
    static PropertySpecs propspecs =
        {PropertySpec("font-caps", "normal"),
         PropertySpec("font-name", "serif"),
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

  const Sosofo& SimplePageSequence::port(const std::string& portName) const
  {
    if (portName == "text") {
      return mTextPort;
    }

    return kNilSosofo;
  }


  //----------------------------------------------------------------------------

  bool isPropertyBeInherited(const std::string& key)
  {
    const auto i_find =
        std::find_if(propertyInherited.begin(), propertyInherited.end(),
                     [&key](const PropertyInherited& propinh) {
          return propinh.mKey == key;
        });
    return i_find != propertyInherited.end() ? i_find->mIsInherited : false;
  }


  //----------------------------------------------------------------------------

  std::unique_ptr<IFormattingObject>
  createFoByClassName(const std::string& className, const PropertySpecs& props,
                      const Sosofo& sosofo)
  {
    if (className == "#literal") {
      return estd::make_unique<Literal>();
    }
    else if (className == "#paragraph-break") {
      return estd::make_unique<ParagraphBreak>();
    }
    else if (className == "#paragraph") {
      return estd::make_unique<Paragraph>(props, sosofo);
    }
    else if (className == "#display-group") {
      return estd::make_unique<DisplayGroup>(props, sosofo);
    }
    else if (className == "#simple-page-sequence") {
      return estd::make_unique<SimplePageSequence>(props, sosofo);
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

    os << "<dimen:" << dimen.mValue << unit_name(dimen.mUnit);
    if (dimen.mValue != dimen.mMin) {
      os << " min " << dimen.mMin << unit_name(dimen.mUnit);
    }
    if (dimen.mMax == std::numeric_limits<double>::infinity()) {
      os << " plus INF";
    }
    else if (dimen.mValue != dimen.mMax) {
      os << " plus " << dimen.mMax << unit_name(dimen.mUnit);
    }

    os << ">";

    return os;
  }

} // ns fo
} // ns eyestep
