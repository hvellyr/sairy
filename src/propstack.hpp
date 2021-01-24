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
    FoProps() = default;
    FoProps(const std::string& str, const IFormattingObject* fo,
            const fo::PropertySpecs& p, const fo::PropertySpecs& dp)
      : _fo_name(str)
      , _fo(fo)
      , _props(p)
      , _default_props(dp) {}
    FoProps(const FoProps& rhs)
      : _fo_name(rhs._fo_name)
      , _fo(rhs._fo)
      , _props(rhs._props)
      , _default_props(rhs._default_props) {}
    std::string _fo_name;
    const IFormattingObject* _fo = nullptr;
    fo::PropertySpecs _props;
    fo::PropertySpecs _default_props;
  };

  std::list<FoProps> _stack;
  fo::IProperties* _prop_lookup = nullptr;
  mutable std::map<std::string, int> _inheritance_level;

public:
  void set_property_lookup(fo::IProperties* prop_lookup);

  void push(const IFormattingObject* fo);
  void pop();

  fo::ValueType get_default(const std::string& key) const;
  fo::ValueType lookup(const std::string& key) const;
  fo::ValueType lookup_inherited(const std::string& key) const;

  template <typename T>
  estd::optional<T> lookup_or_none(const std::string& key) const;
};

} // namespace eyestep

#include "propstack.ipp"
