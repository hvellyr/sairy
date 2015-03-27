// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "abstract-processor.hpp"

#include <boost/filesystem.hpp>

#include <string>


namespace eyestep {

class Sosofo;
class IFormattingObject;
template <typename T>
class IFoProcessor;


class DebugProcessor : public AbstractProcessor<DebugProcessor> {
  using Super = AbstractProcessor;

public:
  std::string procId() const override;
  std::string default_output_extension() const override;

  const IFoProcessor<DebugProcessor>*
  lookupFoProcessor(const std::string& foClassName) const override;

  void beforeRendering() override;
  void renderSosofo(const Sosofo* sosofo) override;
  void renderFo(const IFormattingObject* fo) override;
};

} // ns eyestep
