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
    mProperties[propName] = NodeList{std::make_shared<Node>(child)};
  }
  else {
    if (NodeList* nl = boost::get<NodeList>(&i_find->second)) {
      nl->emplace_back(std::make_shared<Node>(child));
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

    void operator()(const std::shared_ptr<Node>& nd)
    {
      mResult.emplace_back(nd);
    }

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


NodeList Node::children(const PropertyFilterFunc& propFilter) const
{
  NodeList result;
  for (const auto& prop : mProperties) {
    if (propFilter(prop.first)) {
      extractChildren(result, prop.second);
    }
  }
  return result;
}


NodeList Node::children() const
{
  return children([](const std::string&) { return true; });
}


const Properties& Node::properties() const
{
  return mProperties;
}


TraverseRecursion nodeTraverse(const Node& root,
                               const TraverseNodeVisitor& functor,
                               const PropertyFilterFunc& propFilter, int depth)
{
  TraverseRecursion rec = functor(root, depth);

  if (rec == TraverseRecursion::kRecurse) {
    auto children = root.children(propFilter);

    for (const auto& nd : children) {
      TraverseRecursion rec2 =
          nodeTraverse(*nd, functor, propFilter, depth + 1);
      if (rec2 == TraverseRecursion::kBreak) {
        return rec2;
      }
    }
  }

  return rec;
}


TraverseRecursion nodeTraverse(const Node& root,
                               const TraverseNodeVisitor& functor)
{
  return nodeTraverse(root, functor, [](const std::string&) { return true; },
                      0);
}


namespace {
  class SerializeVisitor : public boost::static_visitor<> {
    std::ostream& mOs;
    int mDepth;

  public:
    SerializeVisitor(std::ostream& os, int depth) : mOs(os), mDepth(depth) {}

    void operator()(const Undefined&) {}

    void operator()(const int& val) { mOs << val; }

    void operator()(const std::string& val) { mOs << val; }

    void operator()(const std::shared_ptr<Node>& nd)
    {
      mOs << std::endl;
      serialize(mOs, *nd, mDepth);
      mOs << std::string((mDepth - 1) * 2, ' ');
    }

    void operator()(const NodeList& nl)
    {
      mOs << std::endl;
      for (const auto& nd : nl) {
        serialize(mOs, *nd, mDepth);
      }
      mOs << std::string((mDepth - 1) * 2, ' ');
    }
  };

} // ns anon

void serialize(std::ostream& os, const Node& nd, int in_depth)
{
  int last_depth = in_depth - 1;

  auto close_node = [&os](int lastd, int depth) {
    for (int i = lastd; i >= depth; --i) {
      os << std::string(i * 2, ' ') << "</node>" << std::endl;
    }
  };

  nodeTraverse(nd, [&os, &last_depth, &close_node](const Node& nd, int depth) {
                     close_node(last_depth, depth);
                     last_depth = depth;

                     os << std::string(depth * 2, ' ') << "<node gi='"
                        << nd.gi() << "'>" << std::endl;
                     for (const auto& prop : nd.properties()) {
                       if (prop.first != "children" && prop.first != "gi") {
                         os << std::string((depth + 1) * 2, ' ') << "<prop nm='"
                            << prop.first << "'>";
                         SerializeVisitor visitor(os, depth + 2);
                         boost::apply_visitor(visitor, prop.second);
                         os << "</prop>" << std::endl;
                       }
                     }
                     return TraverseRecursion::kRecurse;
                   },
               [](const std::string& propName) {
                 return propName == "children";
               },
               in_depth);

  close_node(last_depth, in_depth);
}

} // ns eyestep
