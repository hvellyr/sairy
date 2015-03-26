// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/filesystem.hpp>

#include <string>
#include <vector>
#include <unordered_set>


namespace eyestep {

namespace fs = boost::filesystem;

class Node;
class Grove;

class IScanner {
public:
  virtual ~IScanner() {}

  virtual std::string scanner_id() const = 0;
  virtual std::unordered_set<std::string> supported_extensions() const = 0;

  virtual Node* scan_file(eyestep::Grove& grove, const fs::path& srcfile,
                          const std::vector<std::string>& incls,
                          const std::vector<std::string>& defs) = 0;
};

} // ns eyestep
