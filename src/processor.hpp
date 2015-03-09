// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"

#include <boost/filesystem.hpp>

#include <string>


namespace eyestep {

class Sosofo;
class IFormattingObject;

class IProcessor {
public:
  virtual ~IProcessor() {}

  virtual void setOutputFile(const boost::filesystem::path& outputFile) = 0;
  virtual std::string procId() const = 0;
  virtual void renderProcessedNode(const Sosofo* sosofo) = 0;

  virtual void renderSosofo(const Sosofo* sosofo) = 0;
  virtual void renderFo(const IFormattingObject* fo) = 0;

  virtual fo::PropertySpecOrNone property(const IFormattingObject* fo,
                                          const std::string& key) const = 0;
};

} // ns eyestep
