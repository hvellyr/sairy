// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "abstract-processor.hpp"

#include "cxxopts.hpp"

#include <string>


namespace eyestep {

class Sosofo;
class IFormattingObject;
template <typename T>
class IFoProcessor;


class DebugProcessor : public AbstractProcessor<DebugProcessor>
{
  using Super = AbstractProcessor;

public:
  DebugProcessor() = default;
  DebugProcessor(const cxxopts::ParseResult& args);

  std::string proc_id() const override {
    return "debug";
  }

  std::string default_output_extension() const override {
    return {};
  }

  void add_program_options(cxxopts::Options& options) const override;

  const IFoProcessor<DebugProcessor>*
  lookup_fo_processor(const std::string& fo_classname) const override;

  void before_rendering() override;
  void render_sosofo(const Sosofo* sosofo) override;
  void render_fo(const IFormattingObject* fo) override;
};

} // ns eyestep
