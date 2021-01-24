// Copyright (c) 2021 Gregor Klinke
// All rights reserved.

#include "propstack.hpp"


namespace eyestep {

inline void PropertiesStack::set_property_lookup(fo::IProperties* prop_lookup) {
  _prop_lookup = prop_lookup;
}


inline void PropertiesStack::push(const IFormattingObject* fo) {
  _stack.push_front(
    FoProps{fo->classname(), fo, fo->properties(), fo->default_properties()});
}


inline void PropertiesStack::pop() {
  _stack.pop_front();
}


inline fo::ValueType PropertiesStack::get_default(const std::string& key) const {
  if (!_stack.empty()) {
    if (const auto spec = _stack.front()._default_props.lookup_key(key))
      return spec->_value;
  }
  return {};
}


inline fo::ValueType PropertiesStack::lookup_inherited(const std::string& key) const {
  auto i_level = _inheritance_level.find(key);
  auto index = i_level == end(_inheritance_level) ? 0 : i_level->second;

  if (index < _stack.size()) {
    for (auto i_current = std::next(begin(_stack), index); i_current != end(_stack);
         ++i_current) {
      if (const auto spec = i_current->_props.lookup_key(key)) {
        if (const auto expr = fo::get<std::shared_ptr<fo::IExpr>>(spec->_value)) {
          auto prev_idx = _inheritance_level[key];
          _inheritance_level[key] = std::distance(i_current, begin(_stack)) + 1;
          auto result = expr->get()->eval(_prop_lookup);
          if (prev_idx == 0)
            _inheritance_level.erase(key);
          else
            _inheritance_level[key] = prev_idx;
          return result;
        }
        return spec->_value;
      }

      if (!fo::is_property_be_inherited(key))
        break;
    }
  }

  if (!_stack.empty()) {
    if (const auto spec = _stack.front()._default_props.lookup_key(key)) {
      return spec->_value;
    }

    //std::cout << "No default prop for " << key << " in " << _stack.front()._fo_name << "\n";
  }

  return {};
}

inline fo::ValueType PropertiesStack::lookup(const std::string& key) const {
  return lookup_inherited(key);
}


template <typename T>
estd::optional<T> PropertiesStack::lookup_or_none(const std::string& key) const {
  const auto valv = lookup(key);
  if (const T* val = fo::get<const T>(&valv)) {
    return {*val};
  }

  return {};
}

} // namespace eyestep
