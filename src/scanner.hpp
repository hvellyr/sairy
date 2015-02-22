// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <string>
#include <vector>


namespace eyestep {

class Node;


class IScanner {
public:
  virtual ~IScanner() {}

  virtual Node scanFile(const std::string& srcfile,
                        const std::vector<std::string>& incls,
                        const std::vector<std::string>& defs) = 0;
};

} // ns eyestep
