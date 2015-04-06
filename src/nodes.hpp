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
using ConstNodes = std::vector<const Node*>;

struct Undefined {
};

using PropertyValue = boost::variant<Undefined, int, std::string, Node*, Nodes>;
using Properties = std::map<std::string, PropertyValue>;

class Grove;


struct CommonProps {
  static const std::string k_attr_name;
  static const std::string k_attrs;
  static const std::string k_children;
  static const std::string k_data;
  static const std::string k_gi;
  static const std::string k_id;
  static const std::string k_parent;
  static const std::string k_source;
  static const std::string k_value;
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

  Nodes attributes() const;
  void add_attribute(const std::string& attrname, Node* nd);
  void add_attribute(const std::string& attrname, const std::string& value);
  void add_attribute(const std::string& attrname, int value);
  void add_attribute(const std::string& attrname, const Nodes& nl);

  void set_attributes(const Nodes& nl);

  bool has_attribute(const std::string& attrname) const;
  const Node* attribute(const std::string& attrname) const;

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
  Node* make_int_node(int value);

  Node* set_root_node(const NodeClass* node_class);
  Node* root_node() const;

  /*! removes the node from the grove and hands it back to the caller.  The node
    must be referenced by any other node except for its own children! */
  std::unique_ptr<Node> remove_node(Node* nd);
};

/*! Reset the parent reference of all @p nodes.  Use with care. */
void unparent_nodes(Nodes& nodes);

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
