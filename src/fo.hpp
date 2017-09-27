// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/optional/optional.hpp>
#include <boost/variant/variant.hpp>

#include <initializer_list>
#include <iterator>
#include <map>
#include <memory>
#include <string>
#include <vector>


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
    k_in,
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
               bool conditionalp = true, int priority = 1)
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

  std::ostream& operator<<(std::ostream& os, const LengthSpec& ls);


  enum ColorSpace { kGray, kRGB, kCMYK };

  struct Color {
    Color() = default;
    Color(const Color& other) = default;
    Color(Color&& other) = default;

    Color& operator=(const Color& other) = default;
    Color& operator=(Color&& other) = default;

    ColorSpace _space = kGray;
    union {
      float _gray;
      struct {
        float _red;
        float _green;
        float _blue;
      } _rgb;
      struct {
        float _cyan;
        float _magenta;
        float _yellow;
        float _black;
      } _cmyk;
    };
  };

  inline Color make_gray_color(float v)
  {
    Color co;
    co._space = kGray;
    co._gray = v;
    return co;
  }

  inline Color make_rgb_color(float r, float g, float b)
  {
    Color co;
    co._space = kRGB;
    co._rgb = {r, g, b};
    return co;
  }

  inline Color make_cmyk_color(float c, float m, float y, float b)
  {
    Color co;
    co._space = kCMYK;
    co._cmyk = {c, m, y, b};
    return co;
  }

  std::ostream& operator<<(std::ostream& os, const Color& co);


  class PropertySpec {
  public:
    using ValueType =
      boost::variant<LengthSpec, bool, int, std::string, std::shared_ptr<Sosofo>,
                     Color>;

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

    PropertySpec(std::string name, Color val)
      : _name(std::move(name)), _value(val)
    {
    }

    PropertySpec(const PropertySpec& other) = default;
    PropertySpec(PropertySpec&& other) = default;

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

  class PropertySpecs {
  public:
    using storage_type = std::map<std::string, PropertySpec>;

    class PropertySpecIterator
      : public std::iterator<std::random_access_iterator_tag, PropertySpec> {
      typename storage_type::const_iterator _i_ptr;

    public:
      PropertySpecIterator(typename storage_type::const_iterator i_ptr)
        : _i_ptr(i_ptr)
      {
      }

      PropertySpecIterator(const PropertySpecIterator&) = default;
      PropertySpecIterator& operator=(const PropertySpecIterator&) = default;

      bool operator==(const PropertySpecIterator& other) const
      {
        return _i_ptr == other._i_ptr;
      }

      bool operator!=(const PropertySpecIterator& other) const
      {
        return !(operator==(other));
      }

      PropertySpecIterator& operator++()
      {
        ++_i_ptr;
        return *this;
      }

      PropertySpecIterator& operator--()
      {
        ++_i_ptr;
        return *this;
      }

      PropertySpecIterator operator++(int)
      {
        auto tmp(*this);
        ++_i_ptr;
        return tmp;
      }

      PropertySpecIterator operator--(int)
      {
        auto tmp(*this);
        --_i_ptr;
        return tmp;
      }

      const PropertySpec& operator*() const { return _i_ptr->second; }

      const PropertySpec* operator->() const { return &_i_ptr->second; }
    };

    using const_iterator = PropertySpecIterator;

    PropertySpecs() = default;
    PropertySpecs(const PropertySpecs&) = default;
    PropertySpecs(PropertySpecs&&) = default;

    PropertySpecs(std::initializer_list<PropertySpec> si)
    {
      for (auto const& spec : si) {
        _specs.insert(std::make_pair(spec._name, spec));
      }
    }

    PropertySpecs& operator=(const PropertySpecs&) = default;
    PropertySpecs& operator=(PropertySpecs&&) = default;

    void set(const fo::PropertySpec& spec)
    {
      _specs.insert(std::make_pair(spec._name, spec));
    }

    PropertySpecOrNone lookup_key(const std::string& key) const
    {
      auto i_find = _specs.find(key);
      return i_find != _specs.end() ? PropertySpecOrNone(i_find->second)
                                    : boost::none;
    }

    const_iterator begin() const
    {
      return PropertySpecIterator(_specs.begin());
    }

    const_iterator end() const { return PropertySpecIterator(_specs.end()); }

    storage_type _specs;
  };

  bool is_property_be_inherited(const std::string& key);

} // ns fo

class IFormattingObject {
public:
  virtual ~IFormattingObject() {}

  virtual bool accepts_fo(const Sosofo& fo) const = 0;

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
