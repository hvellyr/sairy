// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodes.hpp"
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


//------------------------------------------------------------------------------

const std::string CommonProps::kParent = "parent";
const std::string CommonProps::kGi = "gi";
const std::string CommonProps::kChildren = "children";


//------------------------------------------------------------------------------

Node::Node(const std::string& gi) : mGrove(nullptr)
{
  mProperties[CommonProps::kGi] = gi;
}

std::string Node::gi() const
{
  return property<std::string>(CommonProps::kGi);
}


Node* Node::parent() const
{
  return property<Node*>(CommonProps::kParent);
}


Grove* Node::grove() const
{
  return mGrove;
}


const PropertyValue Node::operator[](const std::string& propName) const
{
  auto i_find = mProperties.find(propName);
  if (i_find != mProperties.end()) {
    return i_find->second;
  }

  return Undefined();
}


bool Node::hasProperty(const std::string& propName) const
{
  return mProperties.find(propName) != mProperties.end();
}


void Node::setProperty(const std::string& propName, const Nodes& nl)
{
  Nodes newNodes(nl);
  for (auto* nd : newNodes) {
    assert(nd->parent() == nullptr);
    assert(nd->mGrove == mGrove);
    nd->mProperties[CommonProps::kParent] = this;
  }

  mProperties[propName] = newNodes;
}

void Node::setProperty(const std::string& propName, Node* nd)
{
  assert(nd->mGrove == mGrove);
  nd->mProperties[CommonProps::kParent] = this;
  mProperties[propName] = nd;
}


void Node::addChildNode(Node* child)
{
  addNode(CommonProps::kChildren, child);
}


void Node::addNode(const std::string& propName, Node* child)
{
  assert(child->mGrove == mGrove);

  auto i_find = mProperties.find(propName);
  if (i_find == mProperties.end()) {
    child->mProperties[CommonProps::kParent] = this;
    mProperties[propName] = Nodes{child};
  }
  else {
    if (Nodes* nl = boost::get<Nodes>(&i_find->second)) {
      child->mProperties[CommonProps::kParent] = this;
      nl->emplace_back(child);
    }
    else {
      assert(false);
    }
  }
}


namespace {
  class CollectChildrenVisitor : public boost::static_visitor<> {
    Nodes& mResult;

  public:
    CollectChildrenVisitor(Nodes& result) : mResult(result) {}

    void operator()(const Undefined&) {}

    void operator()(const int&) {}

    void operator()(const std::string&) {}

    void operator()(Node* nd) { mResult.emplace_back(nd); }

    void operator()(const Nodes& nl)
    {
      for (auto* nd : nl) {
        mResult.emplace_back(nd);
      }
    }
  };

  void extractChildren(Nodes& result, PropertyValue value)
  {
    CollectChildrenVisitor visitor(result);
    boost::apply_visitor(visitor, value);
  }

} // ns anon


Nodes Node::children(const PropertyFilterFunc& propFilter) const
{
  Nodes result;
  for (const auto& prop : mProperties) {
    if (prop.first != CommonProps::kGi && prop.first != CommonProps::kParent &&
        propFilter(prop.first)) {
      extractChildren(result, prop.second);
    }
  }
  return result;
}


Nodes Node::children() const
{
  return children([](const std::string& propName) {
    return propName != CommonProps::kGi && propName != CommonProps::kParent;
  });
}


const Properties& Node::properties() const
{
  return mProperties;
}


//------------------------------------------------------------------------------

Node* Grove::makeNode(const std::string& gi)
{
  auto nd = estd::make_unique<Node>(gi);
  nd->mGrove = this;

  mNodes.emplace_back(std::move(nd));

  return mNodes[mNodes.size() - 1].get();
}

Node* Grove::setRootNode(const std::string& gi)
{
  assert(mNodes.empty());
  return makeNode(gi);
}


Node* Grove::rootNode() const
{
  if (!mNodes.empty()) {
    return mNodes[0].get();
  }
  return nullptr;
}


//------------------------------------------------------------------------------

TraverseRecursion nodeTraverse(const Node* root,
                               const TraverseNodeVisitor& functor,
                               const PropertyFilterFunc& propFilter, int depth)
{
  TraverseRecursion rec = functor(root, depth);

  if (rec == TraverseRecursion::kRecurse) {
    auto children = root->children(propFilter);

    for (const auto& nd : children) {
      TraverseRecursion rec2 = nodeTraverse(nd, functor, propFilter, depth + 1);
      if (rec2 == TraverseRecursion::kBreak) {
        return rec2;
      }
    }
  }

  return rec;
}


TraverseRecursion nodeTraverse(const Node* root,
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

    void operator()(const Node* nd)
    {
      mOs << std::endl;
      serialize(mOs, nd, mDepth);
      mOs << std::string((mDepth - 1) * 2, ' ');
    }

    void operator()(const Nodes& nl)
    {
      mOs << std::endl;
      for (auto* nd : nl) {
        serialize(mOs, nd, mDepth);
      }
      mOs << std::string((mDepth - 1) * 2, ' ');
    }
  };

} // ns anon

void serialize(std::ostream& os, const Node* nd, int in_depth)
{
  int last_depth = in_depth - 1;

  auto close_node = [&os](int lastd, int depth) {
    for (int i = lastd; i >= depth; --i) {
      os << std::string(i * 2, ' ') << "</node>" << std::endl;
    }
  };

  nodeTraverse(nd, [&os, &last_depth, &close_node](const Node* nd, int depth) {
                     close_node(last_depth, depth);
                     last_depth = depth;

                     os << std::string(depth * 2, ' ') << "<node gi='"
                        << nd->gi() << "'>" << std::endl;
                     for (const auto& prop : nd->properties()) {
                       if (prop.first != CommonProps::kChildren &&
                           prop.first != CommonProps::kGi &&
                           prop.first != CommonProps::kParent) {
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
                 return propName == CommonProps::kChildren;
               },
               in_depth);

  close_node(last_depth, in_depth);
}

} // ns eyestep
