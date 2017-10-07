// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

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

struct Undefined
{};


struct PropertyValue
{
  enum Kind
  {
    k_undefined,
    k_int,
    k_string,
    k_nodeptr,
    k_nodes
  };

  Kind _kind;
  union
  {
    Undefined _undefined;
    int _int;
    std::string _string;
    Node* _nodeptr;
    Nodes _nodes;
  };

  PropertyValue()
    : _kind(k_undefined) {}
  PropertyValue(Undefined)
    : _kind(k_undefined) {}
  PropertyValue(int val)
    : _kind(k_int)
    , _int(val) {}
  PropertyValue(std::string val)
    : _kind(k_string)
    , _string(std::move(val)) {}
  PropertyValue(Node* val)
    : _kind(k_nodeptr)
    , _nodeptr(val) {}
  PropertyValue(Nodes val)
    : _kind(k_nodes)
    , _nodes(std::move(val)) {}

  PropertyValue(const PropertyValue& rhs) {
    *this = rhs;
  }

  ~PropertyValue() {
    clear();
  }

  void clear() {
    switch (_kind) {
    case k_undefined:
    case k_int:
    case k_nodeptr:
      break;
    case k_string:
      _string.~basic_string<char>();
      break;
    case k_nodes:
      _nodes.~Nodes();
      break;
    }
    _kind = k_undefined;
  }

  PropertyValue& operator=(const PropertyValue& rhs) {
    if (this != &rhs) {
      clear();

      _kind = rhs._kind;

      switch (rhs._kind) {
      case k_undefined:
        break;
      case k_int:
        _int = rhs._int;
        break;
      case k_nodeptr:
        _nodeptr = rhs._nodeptr;
        break;
      case k_string:
        new (&_string) std::string(rhs._string);
        break;
      case k_nodes:
        new (&_nodes) Nodes(rhs._nodes);
        break;
      }
    }

    return *this;
  }
};


template <typename T>
struct PropertyTrait
{
  static PropertyValue::Kind value_type();
  static const T* get_const(const PropertyValue* val);
  static T* get(PropertyValue* val);
};


template <>
struct PropertyTrait<Undefined>
{
  static PropertyValue::Kind value_type() {
    return PropertyValue::k_undefined;
  }
  static const Undefined* get_const(const PropertyValue* val) {
    return &val->_undefined;
  }
  static Undefined* get(PropertyValue* val) {
    return &val->_undefined;
  }
};


template <>
struct PropertyTrait<int>
{
  static PropertyValue::Kind value_type() {
    return PropertyValue::k_int;
  }
  static const int* get_const(const PropertyValue* val) {
    return &val->_int;
  }
  static int* get(PropertyValue* val) {
    return &val->_int;
  }
};


template <>
struct PropertyTrait<std::string>
{
  static PropertyValue::Kind value_type() {
    return PropertyValue::k_string;
  }
  static const std::string* get_const(const PropertyValue* val) {
    return &val->_string;
  }
  static std::string* get(PropertyValue* val) {
    return &val->_string;
  }
};


template <>
struct PropertyTrait<Node*>
{
  static PropertyValue::Kind value_type() {
    return PropertyValue::k_nodeptr;
  }
  static Node* const* get_const(const PropertyValue* val) {
    return &val->_nodeptr;
  }
  static Node** get(PropertyValue* val) {
    return &val->_nodeptr;
  }
};


template <>
struct PropertyTrait<Nodes>
{
  static PropertyValue::Kind value_type() {
    return PropertyValue::k_nodes;
  }
  static const Nodes* get_const(const PropertyValue* val) {
    return &val->_nodes;
  }
  static Nodes* get(PropertyValue* val) {
    return &val->_nodes;
  }
};


template <typename T>
const T* get(const PropertyValue* val) {
  const auto foo = PropertyTrait<typename std::remove_const<T>::type>::value_type();
  if (val && foo == val->_kind) {
    return PropertyTrait<typename std::remove_const<T>::type>::get_const(val);
  }

  return nullptr;
}


template <typename T>
T* get(PropertyValue* val) {
  return val &&
             PropertyTrait<typename std::remove_const<T>::type>::value_type() ==
               val->_kind
           ? PropertyTrait<typename std::remove_const<T>::type>::get(val)
           : nullptr;
}


template <typename F, typename... Args,
          typename R = typename std::remove_reference<F>::type::return_type>
R apply(F&& f, const PropertyValue& val, Args&&... args) {
  switch (val._kind) {
  case PropertyValue::k_undefined:
    return f(val._undefined, std::forward<Args>(args)...);
  case PropertyValue::k_int:
    return f(val._int, std::forward<Args>(args)...);
  case PropertyValue::k_string:
    return f(val._string, std::forward<Args>(args)...);
  case PropertyValue::k_nodeptr:
    return f(val._nodeptr, std::forward<Args>(args)...);
  case PropertyValue::k_nodes:
    return f(val._nodes, std::forward<Args>(args)...);
  }

  return R();
}


using Properties = std::map<std::string, PropertyValue>;

class Grove;


struct CommonProps
{
  static const std::string k_attr_name;
  static const std::string k_attrs;
  static const std::string k_children;
  static const std::string k_data;
  static const std::string k_desc;
  static const std::string k_gi;
  static const std::string k_id;
  static const std::string k_auto_id;
  static const std::string k_parent;
  static const std::string k_source;
  static const std::string k_value;
  static const std::string k_data_attr;
};


class Node
{
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
  T property(const std::string& propname) const {
    const auto i_find = _properties.find(propname);
    if (i_find != _properties.end()) {
      if (const T* value = get<T>(&i_find->second)) {
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
std::ostream& operator<<(std::ostream& os, const PropertyValue&);


class Grove
{
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

  const std::vector<std::unique_ptr<Node>>& nodes() const {
    return _nodes;
  }
};


/*! Reset the parent reference of all @p nodes.  Use with care. */
void unparent_nodes(Nodes& nodes);

std::string effective_id(const Node& nd);

} // ns eyestep
