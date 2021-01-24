// Copyright (c) 2021 Gregor Klinke
// All rights reserved.

#include "abstract-processor.hpp"


namespace eyestep {
fo::ValueType detail::PropertyLookup::get(const std::string& key) const {
  return _props->lookup_inherited(key);
}


fo::ValueType detail::PropertyLookup::get_default(const std::string& key) const {
  return _props->get_default(key);
}

} // namespace eyestep
