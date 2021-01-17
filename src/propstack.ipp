// Copyright (c) 2021 Gregor Klinke
// All rights reserved.

#include "propstack.hpp"


namespace eyestep {

inline void PropertiesStack::push(const IFormattingObject* fo) {
  push(fo->properties(), fo->default_properties());
}


inline void PropertiesStack::push(const fo::PropertySpecs& props,
                                  const fo::PropertySpecs& default_props) {
  _stack.push_front(FoProps{props, default_props});
}


inline void PropertiesStack::pop() {
  _stack.pop_front();
}


template <typename T>
estd::optional<T> PropertiesStack::lookup(const std::string& key) const {
  for (auto i_current = begin(_stack); i_current != end(_stack); ++i_current) {
    if (const auto spec = i_current->_props.lookup_key(key)) {
      if (const auto* expr = fo::get<const std::shared_ptr<fo::IExpr>>(&spec->_value)) {
        auto valv = expr->get()->eval();
        if (const T* val = fo::get<const T>(&valv)) {
          return *val;
        }
      }
      else if (const T* val = fo::get<const T>(&spec->_value)) {
        return *val;
      }
    }

    if (!fo::is_property_be_inherited(key)) {
      break;
    }
  }

  if (!_stack.empty()) {
    return _stack.front()._default_props.lookup_value<T>(key);
  }

  return {};
}

} // namespace eyestep
