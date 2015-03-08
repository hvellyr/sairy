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
  };

  struct Dimen {
    Dimen() : mValue(0), mUnit(k_pt), mMin(0), mMax(0) {}
    Dimen(double value, Unit unit, boost::optional<double> min = boost::none,
          boost::optional<double> max = boost::none)
        : mValue(value), mUnit(unit), mMin(min != boost::none ? *min : value),
          mMax(max != boost::none ? *max : value)
    {
    }

    Dimen(const Dimen& other)
      : mValue(other.mValue),
        mUnit(other.mUnit),
        mMin(other.mMin),
        mMax(other.mMax)
    {}

    Dimen& operator=(const Dimen& other)
    {
      const_cast<double&>(mValue) = other.mValue;
      const_cast<Unit&>(mUnit) = other.mUnit;
      const_cast<double&>(mMin) = other.mMin;
      const_cast<double&>(mMax) = other.mMax;
      return *this;
    }

    const double mValue;
    const Unit mUnit;
    const double mMin;
    const double mMax;
  };


  struct PropertySpec {
    using ValueType = boost::variant<Dimen, bool, int, std::string, std::shared_ptr<Sosofo>>;

    PropertySpec(std::string name, Dimen val)
        : mName(std::move(name)), mValue(val)
    {
    }

    PropertySpec(std::string name, bool val)
        : mName(std::move(name)), mValue(val)
    {
    }

    PropertySpec(std::string name, int val)
        : mName(std::move(name)), mValue(val)
    {
    }

    PropertySpec(std::string name, std::string val)
        : mName(std::move(name)), mValue(std::move(val))
    {
    }

    PropertySpec(std::string name, std::shared_ptr<Sosofo> val)
        : mName(std::move(name)), mValue(std::move(val))
    {
    }

    PropertySpec(const PropertySpec& other)
      : mName(other.mName),
        mValue(other.mValue)
    {}

    PropertySpec& operator=(const PropertySpec& other)
    {
      const_cast<std::string&>(mName) = other.mName;
      const_cast<ValueType&>(mValue) = other.mValue;
      return *this;
    }

    const std::string mName;
    const ValueType mValue;
  };

} // ns fo

class IFormattingObject {
public:
  virtual ~IFormattingObject() {}

  /*! Returns the class name for this FOs @p class. */
  virtual std::string className() const = 0;

  /*! Return the set of defined properties */
  virtual const std::vector<fo::PropertySpec>& propertiesSpec() const = 0;

  /*! Return a port by @p portName */
  virtual const Sosofo& port(const std::string& portName) const = 0;
};


namespace fo {
  std::unique_ptr<IFormattingObject>
  createFoByClassName(const std::string& className, const PropertySpecs& props,
                      const Sosofo& sosofo);
}

} // ns eyestep
