// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"

#include "fspp/estd/optional.hpp"

#include <list>
#include <memory>
#include <string>


namespace eyestep {

class PropertiesStack
{
  struct FoProps
  {
    fo::PropertySpecs _props;
    fo::PropertySpecs _default_props;
  };

  std::list<FoProps> _stack;

public:
  void push(const IFormattingObject* fo);
  void push(const fo::PropertySpecs& props, const fo::PropertySpecs& default_props = {});
  void pop();

  fo::ValueType lookup(const std::string& key) const;

  template <typename T>
  estd::optional<T> lookup_or_none(const std::string& key) const;
};

} // namespace eyestep

#include "propstack.ipp"
