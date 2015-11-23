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
    PropertySpecs _props;
    Fo() = default;
    Fo(const PropertySpecs& props) : _props(props) {}

  public:
    bool accepts_fo(const Sosofo& fo) const override { return true; }
    const PropertySpecs& properties() const override { return _props; }
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portname) const override;
  };


  class Literal : public Fo {
  public:
    Literal() = default;
    Literal(const PropertySpecs& props);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
    const Sosofo& port(const std::string& portname) const override;

    std::string text() const;
  };


  class Paragraph : public Fo {
    const Sosofo _text_port;

  public:
    Paragraph() = default;
    Paragraph(const PropertySpecs& props, const Sosofo& sosofo);

    bool accepts_fo(const Sosofo& fo) const override;
    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portname) const override;
  };


  class ParagraphBreak : public Fo {
  public:
    ParagraphBreak() = default;

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
  };


  class DisplayGroup : public Fo {
    const Sosofo _text_port;

  public:
    DisplayGroup() = default;
    DisplayGroup(const PropertySpecs& props, const Sosofo& sosofo);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portname) const override;
  };


  class Sequence : public Fo {
    const Sosofo _text_port;

  public:
    Sequence() = default;
    Sequence(const PropertySpecs& props, const Sosofo& sosofo);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portname) const override;
  };


  class LineField : public Fo {
    const Sosofo _text_port;

  public:
    LineField() = default;
    LineField(const PropertySpecs& props, const Sosofo& sosofo);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portname) const override;
  };


  class Score : public Fo {
    const Sosofo _text_port;

  public:
    Score() = default;
    Score(const PropertySpecs& props, const Sosofo& sosofo);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portname) const override;
  };


  class SimplePageSequence : public Fo {
    const Sosofo _text_port;

  public:
    SimplePageSequence() = default;
    SimplePageSequence(const PropertySpecs& props, const Sosofo& sosofo);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portname) const override;
  };


  class ScrollSequence : public Fo {
    const Sosofo _scroll_port;

  public:
    ScrollSequence() = default;
    ScrollSequence(const PropertySpecs& props, const Sosofo& sosofo);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portname) const override;
  };


  class PageNumber : public Fo {
  public:
    PageNumber() = default;
    PageNumber(const PropertySpecs& props);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
  };


  class Anchor : public Fo {
  public:
    Anchor() = default;
    Anchor(const PropertySpecs& props);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
  };


  class FootNote : public Fo {
    const Sosofo _text_port;

  public:
    FootNote() = default;
    FootNote(const PropertySpecs& props, const Sosofo& sosofo);

    std::string classname() const override;
    const PropertySpecs& default_properties() const override;
    const std::vector<std::string>& ports() const override;
    const Sosofo& port(const std::string& portname) const override;
  };


} // ns fo
} // ns eyestep
