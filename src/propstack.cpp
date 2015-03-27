// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "propstack.hpp"
#include "fo.hpp"

#include <string>
#include <iostream>


namespace eyestep {

void PropertiesStack::push(const fo::PropertySpecs& props)
{
  mStack.push_front(props);
}


void PropertiesStack::pop()
{
  mStack.pop_front();
}


fo::PropertySpecOrNone
PropertiesStack::get(const std::string& key,
                     const fo::PropertySpecs& defaults) const
{
  const auto lookupKey =
    [](const fo::PropertySpecs& props, const std::string& key) {
      return std::find_if(props.begin(), props.end(),
                          [&key](const fo::PropertySpec& spec) {
                            return spec.mName == key;
                          });
    };

  auto i_current = mStack.begin();

  const fo::PropertySpecs* current = &(*i_current);

  while (current) {
    auto i_find = lookupKey(*current, key);
    if (i_find != current->end()) {
      return *i_find;
    }

    ++i_current;
    if (i_current != mStack.end() && fo::isPropertyBeInherited(key)) {
      current = &(*i_current);
    }
    else {
      current = nullptr;
    }
  }

  auto i_find = lookupKey(defaults, key);
  if (i_find != defaults.end()) {
    return *i_find;
  }

  return boost::none;
}

} // ns eyestep
