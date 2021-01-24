// Copyright (c) 2021 Gregor Klinke

#include "fo.hpp"

#include <cmath>
#include <limits>


namespace eyestep {

namespace fo {

  std::ostream& operator<<(std::ostream& os, const LengthSpec& ls) {
    auto unit_name = [](Unit un) {
      switch (un) {
      case k_pc:
        return "pc";
      case k_pt:
        return "pt";
      case k_px:
        return "px";
      case k_m:
        return "m";
      case k_mm:
        return "mm";
      case k_cm:
        return "cm";
      case k_em:
        return "em";
      case k_in:
        return "in";
      }

      return "pt";
    };

    os << "<length-spec:" << ls._value << unit_name(ls._unit);
    if (ls._min) {
      os << " min " << *ls._min << unit_name(ls._unit);
    }
    if (ls._max) {
    if (*ls._max == std::numeric_limits<double>::infinity()) {
      os << " max INF";
    }
    else {
      os << " max " << *ls._max << unit_name(ls._unit);
    }
    }

    os << ">";

    return os;
  }


  std::ostream& operator<<(std::ostream& os, const Address& a) {
    os << "<address:" << (a._local ? "local:" : "") << a._destination << ">";
    return os;
  }


  std::ostream& operator<<(std::ostream& os, const Color& co) {
    os << "<color:";

    switch (co._space) {
    case kRGB:
      os << "rgb:" << co._rgb._red << "," << co._rgb._green << "," << co._rgb._blue;
      break;
    case kCMYK:
      os << "cmyk" << co._cmyk._cyan << "," << co._cmyk._magenta << ","
         << co._cmyk._yellow << "," << co._cmyk._black;
      break;
    case kGray:
      os << "gray:" << co._gray;
      break;
    }

    os << ">";
    return os;
  }


  bool is_convertible_to_pt_unit(fo::Unit unit) {
    switch (unit) {
    case fo::k_cm:
    case fo::k_m:
    case fo::k_mm:
    case fo::k_pt:
    case fo::k_pc:
    case fo::k_in:
      return true;
    case fo::k_em:
    case fo::k_px:
      return false;
    }

    return false;
  }


  double unit_to_factor(fo::Unit unit) {
    switch (unit) {
    case fo::k_cm:
      return 0.01;
    case fo::k_m:
      return 1.0;
    case fo::k_mm:
      return 0.001;
    case fo::k_pt:
      return 0.0003527778;
    case fo::k_in:
      return 0.0254;
    case fo::k_pc:
      return 0.004233333;

    case fo::k_em:
    case fo::k_px:;
    }

    return 1.0;
  }

  namespace {
    estd::optional<double> max_optional(const estd::optional<double>& lhs, const estd::optional<double>& rhs) {
      if (lhs && rhs) {
        return {std::max(*lhs, *rhs)};
      }
      else if (lhs) {
        return lhs;
      }

      return rhs;
    }
  }

  LengthSpec operator+(const LengthSpec& lhs, const LengthSpec& rhs) {
    auto result = LengthSpec{};
    result._spec_type = lhs._spec_type;
    result._conditionalp = lhs._conditionalp | rhs._conditionalp;
    result._priority = std::max(lhs._priority, rhs._priority);

    result._min = max_optional(lhs._min, rhs._min);
    result._max = max_optional(lhs._max, rhs._max);

    result._unit = lhs._unit;

    result._value =
      lhs._value + (rhs._value / unit_to_factor(rhs._unit)) * unit_to_factor(lhs._unit);
    return result;
  }


  LengthSpec operator-(const LengthSpec& lhs, const LengthSpec& rhs) {
    auto result = LengthSpec{};
    result._spec_type = lhs._spec_type;
    result._conditionalp = lhs._conditionalp | rhs._conditionalp;
    result._priority = std::max(lhs._priority, rhs._priority);

    result._min = max_optional(lhs._min, rhs._min);
    result._max = max_optional(lhs._max, rhs._max);

    result._unit = lhs._unit;

    result._value =
      lhs._value - (rhs._value / unit_to_factor(rhs._unit)) * unit_to_factor(lhs._unit);
    return result;
  }
} // namespace fo
} // namespace eyestep
