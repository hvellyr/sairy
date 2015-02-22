// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/variant/variant.hpp>
#include <boost/variant/recursive_wrapper.hpp>
#include <boost/variant/get.hpp>

#include <iostream>
#include <map>
#include <string>
#include <vector>


namespace eyestep {

class Node;

using NodeList = std::vector<Node>;

struct Undefined {
};

using PropertyValue =
    boost::variant<Undefined, int, std::string, boost::recursive_wrapper<Node>,
                   boost::recursive_wrapper<NodeList>>;


class Node {
  std::map<std::string, PropertyValue> mProperties;

public:
  Node() = default;
  Node(const Node& other) = default;
  Node(Node&& other) = default;
  Node& operator=(const Node& other) = default;
  Node& operator=(Node&& other) = default;

  Node(const std::string& gi) { mProperties["gi"] = gi; }

  std::string gi() const { return property<std::string>("gi"); }

  PropertyValue operator[](const char* propName) const
  {
    auto i_find = mProperties.find(propName);
    if (i_find != mProperties.end()) {
      return i_find->second;
    }

    return Undefined();
  }

  PropertyValue& operator[](const char* propName)
  {
    return mProperties[propName];
  }

  template <typename T>
  T property(const char* propName) const
  {
    auto i_find = mProperties.find(propName);
    if (i_find != mProperties.end()) {
      if (const T* value = boost::get<T>(&i_find->second)) {
        return *value;
      }
    }

    return T();
  }

  void addChildNode(const Node& child);

  friend std::ostream& operator<<(std::ostream& os, const Node& node);
};

std::ostream& operator<<(std::ostream& os, const Undefined&);
std::ostream& operator<<(std::ostream& os, const NodeList&);

} // ns eyestep
