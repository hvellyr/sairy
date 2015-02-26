// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/filesystem.hpp>
#include <memory>


namespace eyestep {

class Node;


class ISchemeContext {
public:
  virtual ~ISchemeContext(){};

  virtual void initialize(const boost::filesystem::path& modulePath) = 0;
  virtual bool loadScript(const boost::filesystem::path& scriptFile) = 0;

  virtual void setupTemplateFunctions(const eyestep::Node* rootNode) = 0;
};

std::unique_ptr<ISchemeContext> createSchemeContext();

} // ns eyestep
