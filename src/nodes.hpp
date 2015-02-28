// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/variant/get.hpp>
#include <boost/variant/recursive_wrapper.hpp>
#include <boost/variant/variant.hpp>

#include <cassert>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>


namespace eyestep {

class Node;

using Nodes = std::vector<Node*>;

struct Undefined {
};

using PropertyValue = boost::variant<Undefined, int, std::string, Node*, Nodes>;
using Properties = std::map<std::string, PropertyValue>;

using PropertyFilterFunc = std::function<bool(const std::string&)>;

class Grove;


struct CommonProps {
  static const std::string kParent;
  static const std::string kGi;
  static const std::string kChildren;
};


class Node {
  Properties mProperties;
  Grove* mGrove;

public:
  Node() : mGrove(nullptr){};
  Node(const Node& other) = default;
  Node(Node&& other) = default;
  Node& operator=(const Node& other) = default;
  Node& operator=(Node&& other) = default;

  Node(const std::string& gi);

  std::string gi() const;
  Node* parent() const;
  Grove* grove() const;

  const PropertyValue operator[](const std::string& propName) const;

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

  bool hasProperty(const std::string& propName) const;

  template <typename T>
  void setProperty(const std::string& propName, const T& value)
  {
    mProperties[propName] = value;
  }

  void setProperty(const std::string& propName, const Nodes& nl);
  void setProperty(const std::string& propName, Node* nd);

  void addChildNode(Node* child);
  void addNode(const std::string& propName, Node* child);

  Nodes children() const;
  Nodes children(const PropertyFilterFunc& propFilter) const;
  const Properties& properties() const;

  friend std::ostream& operator<<(std::ostream& os, const Node& node);

  friend class Grove;
};

std::ostream& operator<<(std::ostream& os, const Undefined&);
std::ostream& operator<<(std::ostream& os, const Nodes&);


class Grove {
  std::vector<std::unique_ptr<Node>> mNodes;

public:
  Node* makeNode(const std::string& gi);

  Node* setRootNode(const std::string& gi);
  Node* rootNode() const;
};


enum class TraverseRecursion {
  kBreak,
  kContinue,
  kRecurse,
};

using TraverseNodeVisitor =
    std::function<TraverseRecursion(const Node*, int depth)>;

TraverseRecursion nodeTraverse(const Node* root,
                               const TraverseNodeVisitor& functor,
                               const PropertyFilterFunc& propFilter, int depth);

TraverseRecursion nodeTraverse(const Node* root,
                               const TraverseNodeVisitor& functor);

void serialize(std::ostream& os, const Node* nd, int depth = 0);

} // ns eyestep
