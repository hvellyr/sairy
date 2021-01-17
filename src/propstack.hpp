// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"

#include <list>
#include <string>


namespace eyestep {

class PropertiesStack
{
  std::list<fo::PropertySpecs> _stack;

public:
  void push(const fo::PropertySpecs& props);
  void pop();

  fo::PropertySpecOrNone get(const std::string& key,
                             const fo::PropertySpecs& defaults) const;
};

} // namespace eyestep
