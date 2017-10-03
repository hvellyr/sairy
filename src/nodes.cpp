// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodes.hpp"
#include "nodeclass.hpp"
#include "estd/memory.hpp"

#include <algorithm>
#include <cassert>
#include <iostream>


namespace eyestep {

std::ostream& operator<<(std::ostream& os, const Undefined&)
{
  os << "<undefined property>" << std::endl;
  return os;
}


std::ostream& operator<<(std::ostream& os, const Nodes& nodelist)
{
  os << "[ " << std::endl;
  for (const auto* nd : nodelist) {
    os << *nd << std::endl;
  }
  os << "]";
  return os;
}


std::ostream& operator<<(std::ostream& os, const Node& node)
{
  os << node.classname();
  if (node.node_class() == element_class_definition()) {
    os << " '" << node["gi"] << "'";
  }
  os << " {" << std::endl;

  for (const auto& p : node._properties) {
    if (p.first != CommonProps::k_gi && p.first != CommonProps::k_parent) {
      os << "  '" << p.first << "': " << p.second << ";" << std::endl;
    }
  }
  os << "}";

  return os;
}


std::ostream& operator<<(std::ostream& os, const PropertyValue& prop)
{
  struct PropValueVisitor {
    using return_type = void;

    void operator()(const Undefined& val, std::ostream& os) { os << val; }
    void operator()(const int& val, std::ostream& os) { os << val; }
    void operator()(const std::string& val, std::ostream& os) { os << val; }
    void operator()(const Node* val, std::ostream& os) { os << *val; }
    void operator()(const Nodes& val, std::ostream& os) { os << val; }
  };

  apply(PropValueVisitor(), prop, os);
  return os;
}


//------------------------------------------------------------------------------

const std::string CommonProps::k_attr_name = "#attr-name";
const std::string CommonProps::k_attrs = "attributes";
const std::string CommonProps::k_children = "children";
const std::string CommonProps::k_data = "data";
const std::string CommonProps::k_desc = "desc";
const std::string CommonProps::k_gi = "gi";
const std::string CommonProps::k_id = "id";
const std::string CommonProps::k_auto_id = "auto-id";
const std::string CommonProps::k_parent = "parent";
const std::string CommonProps::k_source = "source";
const std::string CommonProps::k_value = "value";
const std::string CommonProps::k_data_attr = "data-attr";


//------------------------------------------------------------------------------

Node::Node() : _grove(nullptr), _class(any_class_definition())
{
}

Node::Node(const NodeClass* node_class) : _grove(nullptr), _class(node_class)
{
}

const std::string& Node::classname() const
{
  assert(_class);
  return _class->_classname;
}

const NodeClass* Node::node_class() const
{
  return _class;
}

std::string Node::gi() const
{
  return property<std::string>(CommonProps::k_gi);
}


Node* Node::parent() const
{
  return property<Node*>(CommonProps::k_parent);
}


Grove* Node::grove() const
{
  return _grove;
}


const PropertyValue Node::operator[](const std::string& propname) const
{
  auto i_find = _properties.find(propname);
  if (i_find != _properties.end()) {
    return i_find->second;
  }

  return {Undefined()};
}


bool Node::has_property(const std::string& propname) const
{
  return _properties.find(propname) != _properties.end();
}


void Node::set_property(const std::string& propname, int value)
{
  assert(find_property(_class, propname) == nullptr ||
         find_property(_class, propname)->_type == PropertyType::k_int);

  _properties[propname] = value;
}

void Node::set_property(const std::string& propname, const std::string& value)
{
  assert(find_property(_class, propname) == nullptr ||
         find_property(_class, propname)->_type == PropertyType::k_string);

  _properties[propname] = value;
}

void Node::set_property(const std::string& propname, const Nodes& nl)
{
  assert(find_property(_class, propname) == nullptr ||
         find_property(_class, propname)->_type == PropertyType::k_nodelist);

  Nodes new_nodes(nl);
  for (auto* nd : new_nodes) {
    assert(nd->parent() == nullptr);
    assert(nd->_grove == _grove);
    nd->_properties[CommonProps::k_parent] = this;
  }

  _properties[propname] = new_nodes;
}

void Node::set_property(const std::string& propname, Node* nd)
{
  assert(find_property(_class, propname) == nullptr ||
         find_property(_class, propname)->_type == PropertyType::k_node);
  assert(!nd || nd->_grove == _grove);

  if (nd) {
    nd->_properties[CommonProps::k_parent] = this;
  }
  _properties[propname] = nd;
}


Nodes Node::attributes() const
{
  return property<Nodes>(CommonProps::k_attrs);
}

void Node::add_attribute(const std::string& attrname, Node* nd)
{
  nd->set_property(CommonProps::k_attr_name, attrname);

  if (!has_property(CommonProps::k_attrs)) {
    Nodes nl;
    nl.push_back(nd);
    set_property(CommonProps::k_attrs, nl);
  }
  else {
    if (Nodes* nds = get<Nodes>(&_properties[CommonProps::k_attrs])) {
      nds->push_back(nd);
    }
  }
}

void Node::add_attribute(const std::string& attrname, const std::string& value)
{
  add_attribute(attrname, grove()->make_text_node(value));
}

void Node::add_attribute(const std::string& attrname, int value)
{
  add_attribute(attrname, grove()->make_int_node(value));
}

void Node::add_attribute(const std::string& attrname, const Nodes& nl)
{
  for (auto* nd : nl) {
    add_attribute(attrname, nd);
  }
}

void Node::set_attributes(const Nodes& nl)
{
#ifndef NDEBUG
  for (const auto nd : nl) {
    assert(nd->has_property(CommonProps::k_attr_name));
  }
#endif
  set_property(CommonProps::k_attrs, nl);
}


bool Node::has_attribute(const std::string& attrname) const
{
  for (const auto& attr : attributes()) {
    if (attr->has_property(CommonProps::k_attr_name) &&
        attr->property<std::string>(CommonProps::k_attr_name) == attrname) {
      return true;
    }
  }

  return false;
}

const Node* Node::attribute(const std::string& attrname) const
{
  for (const auto attrnd : attributes()) {
    if (attrnd->has_property(CommonProps::k_attr_name) &&
        attrnd->property<std::string>(CommonProps::k_attr_name) == attrname) {
      return attrnd;
    }
  }
  return nullptr;
}


void Node::add_child_node(Node* child)
{
  add_node(CommonProps::k_children, child);
}


void Node::add_node(const std::string& propname, Node* child)
{
  assert(child->_grove == _grove);

  auto i_find = _properties.find(propname);
  if (i_find == _properties.end()) {
    child->_properties[CommonProps::k_parent] = this;
    _properties[propname] = Nodes{child};
  }
  else {
    if (Nodes* nl = get<Nodes>(&i_find->second)) {
      child->_properties[CommonProps::k_parent] = this;
      nl->emplace_back(child);
    }
    else {
      assert(false);
    }
  }
}


const Properties& Node::properties() const
{
  return _properties;
}


//------------------------------------------------------------------------------

std::string next_auto_id()
{
  static int counter = 0;
  return std::string("textbook-nd-") + std::to_string(++counter);
}

Node* Grove::make_node(const NodeClass* node_class)
{
  auto nd = estd::make_unique<Node>();

  nd->_grove = this;
  nd->_class = node_class;

  const NodeClass* p = node_class;
  while (p) {
    for (const auto& propspec : node_class->_properties_spec) {
      if (propspec._is_required) {
        switch (propspec._type) {
        case PropertyType::k_int:
          nd->_properties[propspec._name] = 0;
          break;
        case PropertyType::k_string:
          nd->_properties[propspec._name] = std::string();
          break;
        case PropertyType::k_node:
        case PropertyType::k_nodelist:
          break;
        }
      }
    }
    p = p->_super_class;
  }

  nd->set_property(CommonProps::k_auto_id, next_auto_id());

  _nodes.emplace_back(std::move(nd));

  return _nodes[_nodes.size() - 1].get();
}


Node* Grove::make_elt_node(const std::string& gi)
{
  auto* nd = make_node(element_class_definition());
  nd->set_property(CommonProps::k_gi, gi);
  return nd;
}


Node* Grove::make_text_node(const std::string& data)
{
  auto* nd = make_node(text_class_definition());
  nd->set_property(CommonProps::k_data, data);
  return nd;
}


Node* Grove::make_int_node(int value)
{
  auto* nd = make_node(int_class_definition());
  nd->set_property(CommonProps::k_value, value);
  return nd;
}


Node* Grove::set_root_node(const NodeClass* node_class)
{
  assert(_nodes.empty());
  return make_node(node_class);
}


Node* Grove::root_node() const
{
  if (!_nodes.empty()) {
    return _nodes[0].get();
  }
  return nullptr;
}


std::unique_ptr<Node> Grove::remove_node(Node* nd)
{
  auto i_find = std::find_if(_nodes.begin(), _nodes.end(),
                             [&nd](const std::unique_ptr<Node>& node) {
                               return node.get() == nd;
                             });
  if (i_find != _nodes.end()) {
    return std::move(*i_find);
  }

  return nullptr;
}

const std::vector<std::unique_ptr<Node>>& Grove::nodes() const
{
  return _nodes;
}

void unparent_nodes(Nodes& nodes)
{
  for (auto& nd : nodes) {
    nd->set_property(CommonProps::k_parent, nullptr);
  }
}

std::string effective_id(const Node& nd)
{
  if (nd.has_property(CommonProps::k_id))
    return nd.property<std::string>(CommonProps::k_id);
  else if (nd.has_property(CommonProps::k_auto_id))
    return nd.property<std::string>(CommonProps::k_auto_id);

  return "";
}

} // ns eyestep
