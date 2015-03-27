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
  class Fo : public IFormattingObject {
  protected:
    const PropertySpecs mProps;
    Fo() = default;
    Fo(const PropertySpecs& props) : mProps(props) {}

  public:
    const PropertySpecs& properties() const override { return mProps; }
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class Literal : public Fo {
  public:
    Literal() = default;
    Literal(const PropertySpecs& props);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
    const Sosofo& port(const std::string& portName) const override;

    std::string text() const;
  };


  class Paragraph : public Fo {
    const Sosofo mTextPort;

  public:
    Paragraph() = default;
    Paragraph(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class ParagraphBreak : public Fo {
  public:
    ParagraphBreak() = default;

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
  };


  class DisplayGroup : public Fo {
    const Sosofo mTextPort;

  public:
    DisplayGroup() = default;
    DisplayGroup(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class Sequence : public Fo {
    const Sosofo mTextPort;

  public:
    Sequence() = default;
    Sequence(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class LineField : public Fo {
    const Sosofo mTextPort;

  public:
    LineField() = default;
    LineField(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class Score : public Fo {
    const Sosofo mTextPort;

  public:
    Score() = default;
    Score(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class SimplePageSequence : public Fo {
    const Sosofo mTextPort;

  public:
    SimplePageSequence() = default;
    SimplePageSequence(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class ScrollSequence : public Fo {
    const Sosofo mScrollPort;

  public:
    ScrollSequence() = default;
    ScrollSequence(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


  class PageNumber : public Fo {
  public:
    PageNumber() = default;
    PageNumber(const PropertySpecs& props);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
  };


  class Anchor : public Fo {
  public:
    Anchor() = default;
    Anchor(const PropertySpecs& props);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
  };


  class FootNote : public Fo {
    const Sosofo mTextPort;

  public:
    FootNote() = default;
    FootNote(const PropertySpecs& props, const Sosofo& sosofo);

    std::string className() const override;
    const PropertySpecs& defaultProperties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portName) const override;
  };


} // ns fo
} // ns eyestep
