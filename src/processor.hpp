// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

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
};

} // ns eyestep
