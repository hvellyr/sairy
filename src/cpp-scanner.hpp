// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "scanner.hpp"

#include <string>
#include <vector>

namespace eyestep {

class Node;


class CppScanner : public IScanner {
public:
  Node scanFile(const std::string& srfile,
                const std::vector<std::string>& incls,
                const std::vector<std::string>& defs) override;
};

} // ns eyestep
