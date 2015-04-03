// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/optional/optional.hpp>
#include <boost/variant/variant.hpp>

#include <string>
#include <vector>
#include <memory>

namespace eyestep {

class Sosofo;

namespace fo {

  enum Unit : int {
    k_cm,
    k_em,
    k_m,
    k_mm,
    k_pt,
    k_px,
  };

  struct Dimen {
    Dimen() : _value(0), _unit(k_pt), _min(0), _max(0) {}
    Dimen(double value, Unit unit, boost::optional<double> min = boost::none,
          boost::optional<double> max = boost::none)
      : _value(value), _unit(unit), _min(min != boost::none ? *min : value),
        _max(max != boost::none ? *max : value)
    {
    }

    Dimen(const Dimen& other)
      : _value(other._value), _unit(other._unit), _min(other._min),
        _max(other._max)
    {
    }

    Dimen& operator=(const Dimen& other)
    {
      const_cast<double&>(_value) = other._value;
      const_cast<Unit&>(_unit) = other._unit;
      const_cast<double&>(_min) = other._min;
      const_cast<double&>(_max) = other._max;
      return *this;
    }

    const double _value;
    const Unit _unit;
    const double _min;
    const double _max;
  };

  std::ostream& operator<<(std::ostream& os, const Dimen& dimen);


  class PropertySpec {
  public:
    using ValueType =
      boost::variant<Dimen, bool, int, std::string, std::shared_ptr<Sosofo>>;

    PropertySpec(std::string name, Dimen val)
      : _name(std::move(name)), _value(val)
    {
    }

    PropertySpec(std::string name, bool val)
      : _name(std::move(name)), _value(val)
    {
    }

    PropertySpec(std::string name, int val)
      : _name(std::move(name)), _value(val)
    {
    }

    PropertySpec(std::string name, std::string val)
      : _name(std::move(name)), _value(std::move(val))
    {
    }

    PropertySpec(std::string name, std::shared_ptr<Sosofo> val)
      : _name(std::move(name)), _value(std::move(val))
    {
    }

    PropertySpec(const PropertySpec& other)
      : _name(other._name), _value(other._value)
    {
    }

    PropertySpec& operator=(const PropertySpec& other)
    {
      const_cast<std::string&>(_name) = other._name;
      const_cast<ValueType&>(_value) = other._value;
      return *this;
    }

    const std::string _name;
    const ValueType _value;
  };

  using PropertySpecOrNone = boost::optional<fo::PropertySpec>;
  using PropertySpecs = std::vector<PropertySpec>;

  bool is_property_be_inherited(const std::string& key);

} // ns fo

class IFormattingObject {
public:
  virtual ~IFormattingObject() {}

  /*! Returns the class name for this FOs @p class. */
  virtual std::string classname() const = 0;

  /*! Return the set of defined properties */
  virtual const fo::PropertySpecs& default_properties() const = 0;

  virtual const fo::PropertySpecs& properties() const = 0;

  virtual const std::vector<std::string>& ports() const = 0;

  /*! Return a port by @p portName */
  virtual const Sosofo& port(const std::string& portname) const = 0;
};


namespace fo {
  std::unique_ptr<IFormattingObject>
  create_fo_by_classname(const std::string& classname,
                         const PropertySpecs& props, const Sosofo& sosofo);
}

} // ns eyestep
