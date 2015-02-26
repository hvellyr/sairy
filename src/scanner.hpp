// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/filesystem.hpp>

#include <string>
#include <vector>


namespace eyestep {

namespace fs = boost::filesystem;

class Node;
class Grove;

class IScanner {
public:
  virtual ~IScanner() {}

  virtual Node* scanFile(eyestep::Grove& grove, const fs::path& srcfile,
                         const std::vector<std::string>& incls,
                         const std::vector<std::string>& defs) = 0;
};

} // ns eyestep
