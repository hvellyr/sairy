// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"
#include "abstract-processor.hpp"

#include <boost/filesystem/fstream.hpp>

#include <ostream>
#include <string>

namespace eyestep {

class Sosofo;
class IFormattingObject;
template<typename T> class IFoProcessor;


class HtmlProcessor : public AbstractProcessor<HtmlProcessor> {
  boost::filesystem::ofstream mMainStream;

public:
  std::string procId() const override;

  const IFoProcessor<HtmlProcessor>*
  lookupFoProcessor(const std::string& foClassName) const override;

  void beforeRendering() override;
  void afterRendering() override;

  std::ostream& mainStream();

private:
  void writeHtmlProlog();
  void writeHtmlEpilog();

};

} // ns eyestep
