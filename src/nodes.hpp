// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/variant/get.hpp>
#include <boost/variant/recursive_wrapper.hpp>
#include <boost/variant/variant.hpp>

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>


namespace eyestep {

class Node;

using NodeList = std::vector<std::shared_ptr<Node>>;

struct Undefined {
};

using PropertyValue =
    boost::variant<Undefined, int, std::string,
                   boost::recursive_wrapper<std::shared_ptr<Node>>,
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

  PropertyValue operator[](const std::string& propName) const
  {
    auto i_find = mProperties.find(propName);
    if (i_find != mProperties.end()) {
      return i_find->second;
    }

    return Undefined();
  }

  PropertyValue& operator[](const std::string& propName)
  {
    return mProperties[propName];
  }

  template <typename T>
  T property(const std::string& propName) const
  {
    auto i_find = mProperties.find(propName);
    if (i_find != mProperties.end()) {
      if (const T* value = boost::get<T>(&i_find->second)) {
        return *value;
      }
    }

    return T();
  }

  bool hasProperty(const std::string& propName) const
  {
    return mProperties.find(propName) != mProperties.end();
  }

  void setProperty(const std::string& propName, const Node& nd)
  {
    (*this)[propName] = std::make_shared<Node>(nd);
  }

  void addChildNode(const Node& child);
  void addNode(const std::string& propName, const Node& child);

  NodeList children() const;

  friend std::ostream& operator<<(std::ostream& os, const Node& node);
};

std::ostream& operator<<(std::ostream& os, const Undefined&);
std::ostream& operator<<(std::ostream& os, const NodeList&);


void nodeTraverse(const Node& root,
                  const std::function<void(const Node&)>& functor);

} // ns eyestep
