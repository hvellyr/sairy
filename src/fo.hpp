// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fspp/estd/optional.hpp"

#include <initializer_list>
#include <iterator>
#include <map>
#include <memory>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>


namespace eyestep {

class Sosofo;

namespace fo {

  enum Unit : int
  {
    k_cm,
    k_em,
    k_m,
    k_mm,
    k_pt,
    k_px,
    k_in,
  };


  enum LengthSpecType
  {
    kDimen,
    kInline,
    kDisplay,
  };


  struct LengthSpec
  {
    LengthSpec() = default;
    LengthSpec(LengthSpecType spec_type, double value, Unit unit,
               estd::optional<double> min = {}, estd::optional<double> max = {},
               bool conditionalp = true, int priority = 1)
      : _spec_type(spec_type)
      , _conditionalp(conditionalp)
      , _priority(priority)
      , _value(value)
      , _unit(unit)
      , _min(min ? *min : value)
      , _max(max ? *max : value) {}

    LengthSpecType _spec_type = kDimen;
    bool _conditionalp = false;
    int _priority = 0;
    double _value = 0;
    Unit _unit = k_pt;
    double _min = 0;
    double _max = 0;
  };


  std::ostream& operator<<(std::ostream& os, const LengthSpec& ls);


  enum ColorSpace
  {
    kGray,
    kRGB,
    kCMYK
  };


  struct Color
  {
    ColorSpace _space = kGray;
    union
    {
      float _gray;
      struct
      {
        float _red;
        float _green;
        float _blue;
      } _rgb;
      struct
      {
        float _cyan;
        float _magenta;
        float _yellow;
        float _black;
      } _cmyk;
    };
  };


  inline Color make_gray_color(float v) {
    Color co;
    co._space = kGray;
    co._gray = v;
    return co;
  }


  inline Color make_rgb_color(float r, float g, float b) {
    Color co;
    co._space = kRGB;
    co._rgb = {r, g, b};
    return co;
  }


  inline Color make_cmyk_color(float c, float m, float y, float b) {
    Color co;
    co._space = kCMYK;
    co._cmyk = {c, m, y, b};
    return co;
  }


  std::ostream& operator<<(std::ostream& os, const Color& co);

  class ICompoundValue;


  struct ValueType
  {
    enum Kind
    {
      k_length,
      k_bool,
      k_int,
      k_string,
      k_sosofo,
      k_color,
      k_compound,
    };

    Kind _kind = k_bool;
    union
    {
      bool _bool;
      int _int;
      LengthSpec _length;
      std::string _string;
      std::shared_ptr<Sosofo> _sosofo;
      Color _color;
      std::shared_ptr<ICompoundValue> _compound;
    };

    ValueType()
      : ValueType(false) {}

    ValueType(bool val)
      : _kind(k_bool)
      , _bool(val) {}

    ValueType(int val)
      : _kind(k_int)
      , _int(val) {}

    ValueType(LengthSpec val)
      : _kind(k_length)
      , _length(std::move(val)) {}

    ValueType(std::string val)
      : _kind(k_string)
      , _string(std::move(val)) {}

    ValueType(std::shared_ptr<Sosofo> val)
      : _kind(k_sosofo)
      , _sosofo(std::move(val)) {}

    ValueType(Color val)
      : _kind(k_color)
      , _color(std::move(val)) {}

    ValueType(std::shared_ptr<ICompoundValue> proprec)
      : _kind(k_compound)
      , _compound(std::move(proprec)) {}

    ValueType(const ValueType& rhs) {
      *this = rhs;
    }

    ~ValueType() {
      clear();
    }

    void clear() {
      switch (_kind) {
      case k_bool:
      case k_int:
        break;
      case k_color:
        _color.~Color();
        break;
      case k_length:
        _length.~LengthSpec();
        break;
      case k_string:
        _string.~basic_string<char>();
        break;
      case k_sosofo:
        _sosofo.~shared_ptr<Sosofo>();
        break;
      case k_compound:
        _compound.~shared_ptr<ICompoundValue>();
        break;
      }
      _bool = false;
      _kind = k_bool;
    }

    ValueType& operator=(const ValueType& rhs) {
      if (this != &rhs) {
        clear();

        _kind = rhs._kind;

        switch (rhs._kind) {
        case k_length:
          new (&_length) LengthSpec(rhs._length);
          break;
        case k_bool:
          _bool = rhs._bool;
          break;
        case k_int:
          _int = rhs._int;
          break;
        case k_string:
          new (&_string) std::string(rhs._string);
          break;
        case k_sosofo:
          new (&_sosofo) std::shared_ptr<Sosofo>(rhs._sosofo);
          break;
        case k_color:
          new (&_color) Color(rhs._color);
          break;
        case k_compound:
          new (&_compound) std::shared_ptr<ICompoundValue>(rhs._compound);
          break;
        }
      }

      return *this;
    }
  };


  class ICompoundValue
  {
  public:
    virtual ~ICompoundValue() = default;
    virtual const char* type_id() const = 0;
  };


  template <typename T>
  struct ValueTrait
  {
    static ValueType::Kind value_type();
    static const T* get(const ValueType* val);
  };


  template <>
  struct ValueTrait<bool>
  {
    static ValueType::Kind value_type() {
      return ValueType::k_bool;
    }
    static const bool* get(const ValueType* val) {
      return &val->_bool;
    }
  };


  template <>
  struct ValueTrait<int>
  {
    static ValueType::Kind value_type() {
      return ValueType::k_int;
    }
    static const int* get(const ValueType* val) {
      return &val->_int;
    }
  };


  template <>
  struct ValueTrait<LengthSpec>
  {
    static ValueType::Kind value_type() {
      return ValueType::k_length;
    }
    static const LengthSpec* get(const ValueType* val) {
      return &val->_length;
    }
  };


  template <>
  struct ValueTrait<std::string>
  {
    static ValueType::Kind value_type() {
      return ValueType::k_string;
    }
    static const std::string* get(const ValueType* val) {
      return &val->_string;
    }
  };


  template <>
  struct ValueTrait<std::shared_ptr<Sosofo>>
  {
    static ValueType::Kind value_type() {
      return ValueType::k_sosofo;
    }
    static const std::shared_ptr<Sosofo>* get(const ValueType* val) {
      return &val->_sosofo;
    }
  };


  template <>
  struct ValueTrait<Color>
  {
    static ValueType::Kind value_type() {
      return ValueType::k_color;
    }
    static const Color* get(const ValueType* val) {
      return &val->_color;
    }
  };


  template <>
  struct ValueTrait<std::shared_ptr<ICompoundValue>>
  {
    static ValueType::Kind value_type() {
      return ValueType::k_compound;
    }
    static const std::shared_ptr<ICompoundValue>* get(const ValueType* val) {
      return &val->_compound;
    }
  };


  template <typename T>
  const T* get(const ValueType* val) {
    return val && ValueTrait<typename std::remove_cv<T>::type>::value_type() == val->_kind
             ? ValueTrait<typename std::remove_cv<T>::type>::get(val)
             : nullptr;
  }


  template <typename F, typename R = void>
  R apply(F&& f, const ValueType& val) {
    switch (val._kind) {
    case ValueType::k_length:
      return f(val._length);
    case ValueType::k_bool:
      return f(val._bool);
    case ValueType::k_int:
      return f(val._int);
    case ValueType::k_string:
      return f(val._string);
    case ValueType::k_sosofo:
      return f(val._sosofo);
    case ValueType::k_color:
      return f(val._color);
    case ValueType::k_compound:
      return f(val._compound);
    }

    return R();
  }


  class PropertySpec
  {
  public:
    PropertySpec(std::string name, LengthSpec val)
      : _name(std::move(name))
      , _value(std::move(val)) {}

    PropertySpec(std::string name, bool val)
      : _name(std::move(name))
      , _value(val) {}

    PropertySpec(std::string name, int val)
      : _name(std::move(name))
      , _value(val) {}

    PropertySpec(std::string name, std::string val)
      : _name(std::move(name))
      , _value(std::move(val)) {}

    PropertySpec(std::string name, std::shared_ptr<Sosofo> val)
      : _name(std::move(name))
      , _value(std::move(val)) {}

    PropertySpec(std::string name, Color val)
      : _name(std::move(name))
      , _value(std::move(val)) {}

    PropertySpec(std::string name, std::shared_ptr<ICompoundValue> val)
      : _name(std::move(name))
      , _value(std::move(val)) {}

    PropertySpec(const PropertySpec& other) = default;
    PropertySpec(PropertySpec&& other) = default;

    PropertySpec& operator=(const PropertySpec& other) {
      const_cast<std::string&>(_name) = other._name;
      const_cast<ValueType&>(_value) = other._value;
      return *this;
    }

    const std::string _name;
    const ValueType _value;
  };


  using PropertySpecOrNone = estd::optional<fo::PropertySpec>;

  class PropertySpecs
  {
  public:
    using storage_type = std::map<std::string, PropertySpec>;

    class PropertySpecIterator
    {
      typename storage_type::const_iterator _i_ptr;

    public:
      using iterator_category = std::random_access_iterator_tag;
      using value_type = const PropertySpec;
      using difference_type = std::ptrdiff_t;
      using pointer = value_type*;
      using reference = value_type&;

      PropertySpecIterator(typename storage_type::const_iterator i_ptr)
        : _i_ptr(i_ptr) {}

      bool operator==(const PropertySpecIterator& other) const {
        return _i_ptr == other._i_ptr;
      }

      bool operator!=(const PropertySpecIterator& other) const {
        return !(operator==(other));
      }

      PropertySpecIterator& operator++() {
        ++_i_ptr;
        return *this;
      }

      PropertySpecIterator& operator--() {
        ++_i_ptr;
        return *this;
      }

      PropertySpecIterator operator++(int) {
        auto tmp(*this);
        ++_i_ptr;
        return tmp;
      }

      PropertySpecIterator operator--(int) {
        auto tmp(*this);
        --_i_ptr;
        return tmp;
      }

      reference operator*() const {
        return _i_ptr->second;
      }

      pointer operator->() const {
        return &_i_ptr->second;
      }
    };

    using const_iterator = PropertySpecIterator;

    PropertySpecs() = default;
    PropertySpecs(const PropertySpecs&) = default;
    PropertySpecs(PropertySpecs&&) = default;

    PropertySpecs(std::initializer_list<PropertySpec> si) {
      for (auto const& spec : si) {
        _specs.insert(std::make_pair(spec._name, spec));
      }
    }

    PropertySpecs& operator=(const PropertySpecs&) = default;
    PropertySpecs& operator=(PropertySpecs&&) = default;

    void set(const fo::PropertySpec& spec) {
      _specs.insert(std::make_pair(spec._name, spec));
    }

    PropertySpecOrNone lookup_key(const std::string& key) const {
      auto i_find = _specs.find(key);
      return i_find != _specs.end() ? PropertySpecOrNone(i_find->second)
                                    : PropertySpecOrNone();
    }

    template<typename T>
    estd::optional<T> lookup_value(const std::string& key) const {
      if (auto prop = lookup_key(key)) {
        if (auto* val = fo::get<T>(&prop->_value)) {
          return *val;
        }
      }

      return {};
    }

    template<typename T>
    T lookup_value_or(const std::string& key, const T& def) const {
      if (auto val = lookup_value<T>(key)) {
        return *val;
      }
      return def;
    }

    const_iterator begin() const {
      return PropertySpecIterator(_specs.begin());
    }

    const_iterator end() const {
      return PropertySpecIterator(_specs.end());
    }

    storage_type _specs;
  };

  bool is_property_be_inherited(const std::string& key);

} // ns fo


class IFormattingObject
{
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

  virtual void set_port(const std::string& portnm, const Sosofo& sosofo) = 0;
};


namespace fo {
  std::unique_ptr<IFormattingObject> create_fo_by_classname(const std::string& classname,
                                                            const PropertySpecs& props,
                                                            const Sosofo& sosofo);
}

} // ns eyestep
