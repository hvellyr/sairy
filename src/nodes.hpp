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
  static const std::string k_children;
  static const std::string k_data;
  static const std::string k_gi;
  static const std::string k_parent;
  static const std::string k_source;
};


class Node {
  Properties _properties;
  Grove* _grove;
  const NodeClass* _class;

public:
  Node();
  Node(const NodeClass* node_class);
  Node(const Node& other) = default;
  Node(Node&& other) = default;
  Node& operator=(const Node& other) = default;
  Node& operator=(Node&& other) = default;

  const std::string& classname() const;
  const NodeClass* node_class() const;
  std::string gi() const;
  Node* parent() const;
  Grove* grove() const;

  const PropertyValue operator[](const std::string& propname) const;

  template <typename T>
  T property(const std::string& propname) const
  {
    auto i_find = _properties.find(propname);
    if (i_find != _properties.end()) {
      if (const T* value = boost::get<T>(&i_find->second)) {
        return *value;
      }
    }

    return T();
  }

  bool has_property(const std::string& propname) const;

  void set_property(const std::string& propname, int value);
  void set_property(const std::string& propname, const std::string& value);

  void set_property(const std::string& propname, const Nodes& nl);
  void set_property(const std::string& propname, Node* nd);

  void add_child_node(Node* child);
  void add_node(const std::string& propname, Node* child);

  const Properties& properties() const;

  friend std::ostream& operator<<(std::ostream& os, const Node& node);

  friend class Grove;
};

std::ostream& operator<<(std::ostream& os, const Undefined&);
std::ostream& operator<<(std::ostream& os, const Nodes&);


class Grove {
  std::vector<std::unique_ptr<Node>> _nodes;

public:
  Node* make_node(const NodeClass* node_class);
  Node* make_elt_node(const std::string& gi);
  Node* make_text_node(const std::string& data);

  Node* set_root_node(const NodeClass* node_class);
  Node* root_node() const;
};


enum class TraverseRecursion {
  k_break,
  k_continue,
  k_recurse,
};

using TraverseNodeVisitor =
  std::function<TraverseRecursion(const Node*, int depth)>;

TraverseRecursion node_traverse(const Node* root,
                                const TraverseNodeVisitor& functor,
                                int depth = 0);

void serialize(std::ostream& os, const Node* nd, int depth = 0);

} // ns eyestep
