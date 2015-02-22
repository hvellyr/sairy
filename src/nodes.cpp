// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>

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


namespace {
  class CollectChildrenVisitor : public boost::static_visitor<> {
    NodeList& mResult;

  public:
    CollectChildrenVisitor(NodeList& result) : mResult(result) {}

    void operator()(const Undefined&) {}

    void operator()(const int&) {}

    void operator()(const std::string&) {}

    void operator()(const Node& nd) { mResult.emplace_back(nd); }

    void operator()(const NodeList& nl)
    {
      for (const auto& nd : nl) {
        mResult.emplace_back(nd);
      }
    }
  };

  void extractChildren(NodeList& result, PropertyValue value)
  {
    CollectChildrenVisitor visitor(result);
    boost::apply_visitor(visitor, value);
  }

} // ns anon


NodeList Node::children() const
{
  NodeList result;
  for (const auto& prop : mProperties) {
    extractChildren(result, prop.second);
  }
  return result;
}


void nodeTraverse(const Node& root,
                  const std::function<void(const Node&)>& functor)
{
  functor(root);

  for (const auto& nd : root.children()) {
    nodeTraverse(nd, functor);
  }
}

} // ns eyestep
