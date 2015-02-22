// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include <boost/variant/apply_visitor.hpp>

#include "nodes.hpp"
#include <iostream>
#include <cassert>


namespace eyestep {

std::ostream& operator<<(std::ostream& os, const Undefined&)
{
  os << "<undefined property>" << std::endl;
  return os;
}


std::ostream& operator<<(std::ostream& os, const NodeList& nodelist)
{
  os << "[ " << std::endl;
  for (const auto& nd : nodelist) {
    os << nd << std::endl;
  }
  os << "]";
  return os;
}


std::ostream& operator<<(std::ostream& os, const Node& node)
{
  os << "Node '" << node["gi"] << "' {" << std::endl;

  for (const auto& p : node.mProperties) {
    if (p.first != "gi")
      os << "  '" << p.first << "': " << p.second << ";" << std::endl;
  }
  os << "}";

  return os;
}


void Node::addChildNode(const Node& child)
{
  addNode("children", child);
}


void Node::addNode(const std::string& propName, const Node& child)
{
  auto i_find = mProperties.find(propName);
  if (i_find == mProperties.end()) {
    mProperties[propName] = NodeList{child};
  }
  else {
    if (NodeList* nl = boost::get<NodeList>(&i_find->second)) {
      nl->emplace_back(child);
    }
    else {
      assert(false);
    }
  }
}

} // ns eyestep
