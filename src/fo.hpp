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

  enum LengthSpecType {
    kDimen,
    kInline,
    kDisplay,
  };

  struct LengthSpec {
    LengthSpec() : _value(0), _unit(k_pt), _min(0), _max(0) {}
    LengthSpec(LengthSpecType spec_type, double value, Unit unit,
               boost::optional<double> min = boost::none,
               boost::optional<double> max = boost::none,
               bool conditionalp = false, int priority = 1)
      : _spec_type(spec_type), _conditionalp(conditionalp), _priority(priority),
        _value(value), _unit(unit), _min(min != boost::none ? *min : value),
        _max(max != boost::none ? *max : value)
    {
    }

    LengthSpec(const LengthSpec& other) = default;
    LengthSpec& operator=(const LengthSpec& other) = default;

    LengthSpecType _spec_type = kDimen;
    bool _conditionalp;
    int _priority;
    double _value;
    Unit _unit;
    double _min;
    double _max;
  };

  std::ostream& operator<<(std::ostream& os, const LengthSpec& dimen);


  class PropertySpec {
  public:
    using ValueType =
      boost::variant<LengthSpec, bool, int, std::string, std::shared_ptr<Sosofo>>;

    PropertySpec(std::string name, LengthSpec val)
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
