// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "scanner.hpp"

#include <string>
#include <vector>

namespace eyestep {

namespace fs = boost::filesystem;

class Node;
class Grove;

class CppScanner : public IScanner {
public:
  static const std::string kId;

  Node* scan_file(eyestep::Grove& grove, const fs::path& srfile,
                  const std::vector<std::string>& incls,
                  const std::vector<std::string>& defs) override;
};

} // ns eyestep
