// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodes.hpp"
#include "nodeclass.hpp"
#include "estd/memory.hpp"

#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>

#include <iostream>
#include <cassert>


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


//------------------------------------------------------------------------------

const std::string CommonProps::k_children = "children";
const std::string CommonProps::k_data = "data";
const std::string CommonProps::k_gi = "gi";
const std::string CommonProps::k_parent = "parent";
const std::string CommonProps::k_source = "source";


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

  return Undefined();
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
  assert(nd->_grove == _grove);

  nd->_properties[CommonProps::k_parent] = this;
  _properties[propname] = nd;
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
    if (Nodes* nl = boost::get<Nodes>(&i_find->second)) {
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


//------------------------------------------------------------------------------

TraverseRecursion node_traverse(const Node* root,
                                const TraverseNodeVisitor& functor, int depth)
{
  TraverseRecursion rec = functor(root, depth);

  if (rec == TraverseRecursion::k_recurse) {
    const Nodes& nodes = root->property<Nodes>(CommonProps::k_children);

    for (const auto& nd : nodes) {
      TraverseRecursion rec2 = node_traverse(nd, functor, depth + 1);
      if (rec2 == TraverseRecursion::k_break) {
        return rec2;
      }
    }
  }

  return rec;
}


namespace {
  class SerializeVisitor : public boost::static_visitor<> {
    std::ostream& _os;
    int _depth;

  public:
    SerializeVisitor(std::ostream& os, int depth) : _os(os), _depth(depth) {}

    void operator()(const Undefined&) {}

    void operator()(const int& val) { _os << val; }

    void operator()(const std::string& val) { _os << val; }

    void operator()(const Node* nd)
    {
      _os << std::endl;
      serialize(_os, nd, _depth);
      _os << std::string((_depth - 1) * 2, ' ');
    }

    void operator()(const Nodes& nl)
    {
      if (!nl.empty()) {
        _os << std::endl;
        for (auto* nd : nl) {
          serialize(_os, nd, _depth);
        }
        _os << std::string((_depth - 1) * 2, ' ');
      }
    }
  };

} // ns anon


void serialize(std::ostream& os, const Node* nd, int in_depth)
{
  int last_depth = in_depth - 1;

  auto close_node = [&os](const Node* n, int lastd, int depth) {
    for (int i = lastd; i >= depth; --i) {
      os << std::string(i * 2, ' ') << "</" << n->classname() << ">"
         << std::endl;
    }
  };

  node_traverse(nd, [&os, &last_depth, &close_node](const Node* nd, int depth) {
    close_node(nd, last_depth, depth);
    last_depth = depth;

    os << std::string(depth * 2, ' ') << "<" << nd->classname();
    if (nd->node_class() == element_class_definition()) {
      os << " gi='" << nd->gi() << "'";
    }
    os << ">" << std::endl;
    for (const auto& prop : nd->properties()) {
      if (prop.first != CommonProps::k_children &&
          prop.first != CommonProps::k_gi &&
          prop.first != CommonProps::k_parent) {
        os << std::string((depth + 1) * 2, ' ') << "<prop nm='" << prop.first
           << "'>";
        SerializeVisitor visitor(os, depth + 2);
        boost::apply_visitor(visitor, prop.second);
        os << "</prop>" << std::endl;
      }
    }
    return TraverseRecursion::k_recurse;
  }, in_depth);

  close_node(nd, last_depth, in_depth);
}

} // ns eyestep
