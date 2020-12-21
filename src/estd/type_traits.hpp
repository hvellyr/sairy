// Copyright (c) 2020 Gregor Klinke

#pragma once

#include "config.hpp"

#include <type_traits>


namespace eyestep {
namespace estd {

#if !defined(TEXTBOOK_HAVE_STD_ENABLE_IF_T)

template <bool Predicate, typename T = void>
using enable_if_t = typename std::enable_if<Predicate, T>::type;

#else

using std::enable_if_t;

#endif

}  // namespace estd
}  // namespace eyestep
