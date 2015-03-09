// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"
#include "processor.hpp"
#include "propstack.hpp"

#include <boost/filesystem.hpp>

#include <string>


namespace eyestep {

class Sosofo;
class IFormattingObject;
template<typename T> class IFoProcessor;


template<typename ProcessorT>
class AbstractProcessor : public IProcessor {
protected:
  boost::filesystem::path mOutputFile;
  PropertiesStack mProps;

public:
  void setOutputFile(const boost::filesystem::path& outputFile) override;
  void renderProcessedNode(const Sosofo* sosofo) override;

  void renderSosofo(const Sosofo* sosofo) override;
  void renderFo(const IFormattingObject* fo) override;

  virtual const IFoProcessor<ProcessorT>*
  lookupFoProcessor(const std::string& foClassName) const = 0;

  virtual void beforeRendering();
  virtual void afterRendering();

  fo::PropertySpecOrNone property(const IFormattingObject* fo,
                                  const std::string& key) const override;
};

} // ns eyestep

#include "abstract-processor.ipp"
