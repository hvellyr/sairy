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
class NodeClass;

using Nodes = std::vector<Node*>;

struct Undefined {
};

using PropertyValue = boost::variant<Undefined, int, std::string, Node*, Nodes>;
using Properties = std::map<std::string, PropertyValue>;

class Grove;


struct CommonProps {
  static const std::string kParent;
  static const std::string kGi;
  static const std::string kChildren;
};


class Node {
  Properties mProperties;
  Grove* mGrove;
  const NodeClass* mClass;

public:
  Node();
  Node(const NodeClass* nodeClass);
  Node(const Node& other) = default;
  Node(Node&& other) = default;
  Node& operator=(const Node& other) = default;
  Node& operator=(Node&& other) = default;

  const std::string& className() const;
  const NodeClass* nodeClass() const;
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

  void setProperty(const std::string& propName, int value);
  void setProperty(const std::string& propName, const std::string& value);

  void setProperty(const std::string& propName, const Nodes& nl);
  void setProperty(const std::string& propName, Node* nd);

  void addChildNode(Node* child);
  void addNode(const std::string& propName, Node* child);

  const Properties& properties() const;

  friend std::ostream& operator<<(std::ostream& os, const Node& node);

  friend class Grove;
};

std::ostream& operator<<(std::ostream& os, const Undefined&);
std::ostream& operator<<(std::ostream& os, const Nodes&);


class Grove {
  std::vector<std::unique_ptr<Node>> mNodes;

public:
  Node* makeNode(const NodeClass* nodeClass);
  Node* makeEltNode(const std::string& gi);

  Node* setRootNode(const NodeClass* nodeClass);
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
                               int depth = 0);

void serialize(std::ostream& os, const Node* nd, int depth = 0);

} // ns eyestep
