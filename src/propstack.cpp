// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "propstack.hpp"
#include "fo.hpp"

#include <iostream>
#include <string>


namespace eyestep {

void PropertiesStack::push(const fo::PropertySpecs& props) {
  _stack.push_front(props);
}


void PropertiesStack::pop() {
  _stack.pop_front();
}


fo::PropertySpecOrNone PropertiesStack::get(const std::string& key,
                                            const fo::PropertySpecs& defaults) const {
  auto i_current = _stack.begin();

  const auto* current = &(*i_current);

  while (current) {
    if (auto spec = current->lookup_key(key))
      return spec;

    ++i_current;
    if (i_current != _stack.end() && fo::is_property_be_inherited(key)) {
      current = &(*i_current);
    }
    else {
      current = nullptr;
    }
  }

  if (auto spec = defaults.lookup_key(key))
    return spec;

  return {};
}

} // ns eyestep
