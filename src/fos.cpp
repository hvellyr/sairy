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

  static Sosofo kNilSosofo;


  const std::vector<std::string>& Fo::ports() const
  {
    static const auto ports = std::vector<std::string>{
    };
    return ports;
  }


  //----------------------------------------------------------------------------

  Literal::Literal(std::string data) : mData(data) {}

  /*! Returns the class name for this FOs @p class. */
  std::string Literal::className() const { return "#literal"; }

  /*! Return the set of defined properties */
  const PropertySpecs& Literal::propertiesSpec() const
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

  const PropertySpecs& Paragraph::propertiesSpec() const
  {
    double max_inf = std::numeric_limits<double>::max();

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

  const PropertySpecs& ParagraphBreak::propertiesSpec() const
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

  const PropertySpecs& DisplayGroup::propertiesSpec() const
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

  const PropertySpecs& SimplePageSequence::propertiesSpec() const
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
      case k_pt: return "pt";
      case k_m: return "m";
      case k_mm: return "mm";
      case k_cm: return "cm";
      case k_em: return "em";
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
