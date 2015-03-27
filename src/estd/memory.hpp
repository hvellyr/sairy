// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <memory>
#include <type_traits>

namespace estd {

template <class T, class... Args>
auto make_unique(Args&&... args) ->
  typename std::enable_if<!std::is_array<T>::value, std::unique_ptr<T>>::type
{
  return std::unique_ptr<T>{new T(std::forward<Args>(args)...)};
}

template <class T>
auto make_unique(std::size_t size) ->
  typename std::enable_if<std::is_array<T>::value, std::unique_ptr<T>>::type
{
  return std::unique_ptr<T>{new typename std::remove_extent<T>::type[size]()};
}

} // ns estd
