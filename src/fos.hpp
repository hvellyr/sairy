// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"
#include "sosofo.hpp"

#include <boost/variant/variant.hpp>

#include <string>
#include <vector>

namespace eyestep {

class Sosofo;

namespace fo {
  using PropertySpecs = std::vector<PropertySpec>;

  class Fo : public IFormattingObject {
  protected:
    const PropertySpecs mProps;
    Fo() = default;
    Fo(const PropertySpecs& props) : mProps(props) {}

  public:
    const PropertySpecs& properties() const { return mProps; }
  };


  class Literal : public Fo {
    std::string mData;

  public:
    Literal() = default;
    Literal(std::string data);

    std::string className() const override;
    const PropertySpecs& propertiesSpec() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class Paragraph : public Fo {
    const Sosofo mTextPort;

  public:
    Paragraph() = default;
    Paragraph(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& propertiesSpec() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class ParagraphBreak : public Fo {
  public:
    ParagraphBreak() = default;

    std::string className() const override;
    const PropertySpecs& propertiesSpec() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class DisplayGroup : public Fo {
    const Sosofo mTextPort;

  public:
    DisplayGroup() = default;
    DisplayGroup(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& propertiesSpec() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class SimplePageSequence : public Fo {
    const Sosofo mTextPort;

  public:
    SimplePageSequence() = default;
    SimplePageSequence(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& propertiesSpec() const override;
    const Sosofo& port(const std::string& portName) const override;
  };

} // ns fo
} // ns eyestep
