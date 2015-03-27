// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodelist.hpp"
#include "estd/memory.hpp"

#include <memory>
#include <algorithm>
#include <cassert>


namespace eyestep {

using detail::INodeListStrategy;
using detail::NodesNodeListStrategy;
using detail::SiblingsNodeListStrategy;
using detail::AncestorsNodeListStrategy;
using detail::DescendantsNodeListStrategy;
using detail::CompositeNodeListStrategy;


//------------------------------------------------------------------------------

NodeList::NodeList()
{
}


NodeList::NodeList(const Node* nd, Kind kind)
{
  assert(nd);

  if (kind == kChildren) {
    int end = int(nd->property<Nodes>(CommonProps::kChildren).size());
    if (end > 0) {
      mStrategy = estd::make_unique<SiblingsNodeListStrategy>(nd, 0, end);
    }
  }
  else if (kind == kSiblings) {
    const auto* parent = nd->parent();
    int end = int(parent->property<Nodes>(CommonProps::kChildren).size());
    if (end > 0) {
      mStrategy = estd::make_unique<SiblingsNodeListStrategy>(parent, 0, end);
    }
  }
  else if (kind == kPreced) {
    const auto* parent = nd->parent();
    const Nodes& nl = parent->property<Nodes>(CommonProps::kChildren);
    auto i_find = std::find_if(nl.begin(), nl.end(),
                               [&](const Node* n) { return nd == n; });
    int end = std::distance(nl.begin(), i_find);
    if (end > 0) {
      mStrategy = estd::make_unique<SiblingsNodeListStrategy>(parent, 0, end);
    }
  }
  else if (kind == kFollow) {
    const auto* parent = nd->parent();
    const Nodes& nl = parent->property<Nodes>(CommonProps::kChildren);
    auto i_find = std::find_if(nl.begin(), nl.end(),
                               [&](const Node* n) { return nd == n; });
    int start = std::distance(nl.begin(), i_find) + 1;
    int end = int(nl.size());

    if (start < end) {
      mStrategy =
        estd::make_unique<SiblingsNodeListStrategy>(parent, start, end);
    }
  }
  else if (kind == kAncestors) {
    mStrategy = estd::make_unique<AncestorsNodeListStrategy>(nd->parent(), -1);
  }
  else if (kind == kDescendants) {
    int end = int(nd->property<Nodes>(CommonProps::kChildren).size());
    if (end > 0) {
      mStrategy =
        estd::make_unique<DescendantsNodeListStrategy>(nd, 0, end, -1,
                                                       std::vector<int>{});
    }
  }
}


NodeList::NodeList(const std::vector<NodeList>& nl)
{
  std::vector<NodeList> result;
  for (const auto& n : nl) {
    if (!n.empty()) {
      result.emplace_back(n.clone());
    }
  }
  mStrategy =
    estd::make_unique<detail::CompositeNodeListStrategy>(std::move(result), -1);
}


NodeList::NodeList(std::unique_ptr<INodeListStrategy> strategy)
  : mStrategy(std::move(strategy))
{
}


NodeList::NodeList(const std::vector<const Node*>& nodes)
  : mStrategy(estd::make_unique<NodesNodeListStrategy>(nodes, 0))
{
}


NodeList NodeList::clone() const
{
  return mStrategy ? NodeList(mStrategy->clone()) : NodeList();
}


bool NodeList::empty() const
{
  return mStrategy ? mStrategy->empty() : true;
}


int NodeList::length() const
{
  return mStrategy ? mStrategy->length() : 0;
}


const Node* NodeList::head() const
{
  return mStrategy ? mStrategy->head() : nullptr;
}


NodeList NodeList::rest() const
{
  return mStrategy ? mStrategy->rest() : NodeList();
}


//------------------------------------------------------------------------------

NodesNodeListStrategy::NodesNodeListStrategy(
  const std::vector<const Node*>& nodes, int start)
  : mNodes(nodes), mStart(start)
{
}

std::unique_ptr<INodeListStrategy> NodesNodeListStrategy::clone() const
{
  return estd::make_unique<NodesNodeListStrategy>(mNodes, mStart);
}

bool NodesNodeListStrategy::empty() const
{
  return int(mNodes.size()) - mStart == 0;
}

int NodesNodeListStrategy::length() const
{
  return int(mNodes.size()) - mStart;
}

const Node* NodesNodeListStrategy::head() const
{
  assert(mStart < int(mNodes.size()));
  return mNodes[mStart];
}

NodeList NodesNodeListStrategy::rest() const
{
  if (mStart + 1 < int(mNodes.size())) {
    return NodeList(
      std::move(estd::make_unique<NodesNodeListStrategy>(mNodes, mStart + 1)));
  }
  return NodeList();
}


//------------------------------------------------------------------------------

SiblingsNodeListStrategy::SiblingsNodeListStrategy(const Node* node, int start,
                                                   int end)
  : mNode(node), mStart(start), mEnd(end)
{
}

std::unique_ptr<INodeListStrategy> SiblingsNodeListStrategy::clone() const
{
  return estd::make_unique<SiblingsNodeListStrategy>(mNode, mStart, mEnd);
}

bool SiblingsNodeListStrategy::empty() const
{
  return mEnd - mStart == 0;
}

int SiblingsNodeListStrategy::length() const
{
  return mEnd - mStart;
}

const Node* SiblingsNodeListStrategy::head() const
{
  assert(mStart < mEnd);
  return mNode->property<Nodes>(CommonProps::kChildren)[mStart];
}

NodeList SiblingsNodeListStrategy::rest() const
{
  if (mStart + 1 < mEnd) {
    return NodeList(std::move(
      estd::make_unique<SiblingsNodeListStrategy>(mNode, mStart + 1, mEnd)));
  }
  return NodeList();
}


//------------------------------------------------------------------------------

AncestorsNodeListStrategy::AncestorsNodeListStrategy(const Node* parent,
                                                     int count)
  : mNode(parent), mCount(count)
{
}

std::unique_ptr<INodeListStrategy> AncestorsNodeListStrategy::clone() const
{
  return estd::make_unique<AncestorsNodeListStrategy>(mNode, mCount);
}

bool AncestorsNodeListStrategy::empty() const
{
  return mCount == 0 || mNode == nullptr;
}

int AncestorsNodeListStrategy::length() const
{
  if (mCount < 0) {
    const Node* p = mNode;
    int nc = 0;
    while (p) {
      nc++;
      p = p->parent();
    }
    const_cast<AncestorsNodeListStrategy*>(this)->mCount = nc;
  }
  return mCount;
}

const Node* AncestorsNodeListStrategy::head() const
{
  return mNode;
}

NodeList AncestorsNodeListStrategy::rest() const
{
  if (Node* p = mNode->parent()) {
    return NodeList(
      estd::make_unique<AncestorsNodeListStrategy>(p, mCount - 1));
  }
  return NodeList();
}


//------------------------------------------------------------------------------

DescendantsNodeListStrategy::DescendantsNodeListStrategy(const Node* node,
                                                         int start, int end,
                                                         int count,
                                                         std::vector<int> stack)
  : mNode(node), mStart(start), mEnd(end), mCount(count),
    mStack(std::move(stack))
{
}

std::unique_ptr<INodeListStrategy> DescendantsNodeListStrategy::clone() const
{
  return estd::make_unique<DescendantsNodeListStrategy>(mNode, mStart, mEnd,
                                                        mCount, mStack);
}

bool DescendantsNodeListStrategy::empty() const
{
  if (mCount < 0) {
    return length() == 0;
  }
  return mCount == 0;
}

int DescendantsNodeListStrategy::length() const
{
  if (mCount < 0) {
    int nc = 1;
    NodeList nl = rest();
    while (nl.mStrategy.get() != nullptr) {
      nl = nl.rest();
      nc++;
    }

    const_cast<DescendantsNodeListStrategy*>(this)->mCount = nc;
  }
  return mCount;
}

const Node* DescendantsNodeListStrategy::head() const
{
  assert(mStart < mEnd);
  return mNode->property<Nodes>(CommonProps::kChildren)[mStart];
}

NodeList DescendantsNodeListStrategy::rest() const
{
  if (mStart < mEnd) {
    Node* newNode = mNode->property<Nodes>(CommonProps::kChildren)[mStart];

    const Nodes& children = newNode->property<Nodes>(CommonProps::kChildren);
    if (children.size() > 0) {
      std::vector<int> stack(mStack);
      stack.emplace_back(mStart + 1);

      return NodeList(estd::make_unique<
        DescendantsNodeListStrategy>(newNode, 0,
                                     int(newNode->property<Nodes>(
                                                    CommonProps::kChildren)
                                           .size()),
                                     mCount - 1, std::move(stack)));
    }
  }

  if (mStart + 1 < mEnd) {
    return NodeList(
      estd::make_unique<DescendantsNodeListStrategy>(mNode, mStart + 1, mEnd,
                                                     mCount - 1, mStack));
  }

  if (mStack.size() > 0) {
    Node* p = mNode->parent();
    auto i_last = std::prev(mStack.end());

    while (p && i_last >= mStack.begin()) {
      auto end = int(p->property<Nodes>(CommonProps::kChildren).size());
      auto start = *i_last;
      if (start < end) {
        std::vector<int> stack(mStack.begin(), i_last);
        return NodeList(
          estd::make_unique<DescendantsNodeListStrategy>(p, start, end,
                                                         mCount - 1,
                                                         std::move(stack)));
      }

      p = p->parent();
      --i_last;
    }
  }

  return NodeList();
}


//------------------------------------------------------------------------------

CompositeNodeListStrategy::CompositeNodeListStrategy(std::vector<NodeList> nl,
                                                     int count)
  : mNL(std::move(nl)), mCount(count)
{
}

std::unique_ptr<INodeListStrategy> CompositeNodeListStrategy::clone() const
{
  std::vector<NodeList> nl;
  for (const auto& n : mNL) {
    nl.emplace_back(n.clone());
  }
  return estd::make_unique<CompositeNodeListStrategy>(std::move(nl), mCount);
}

bool CompositeNodeListStrategy::empty() const
{
  return mNL.empty();
}

int CompositeNodeListStrategy::length() const
{
  if (mCount < 0) {
    int nc = 0;
    for (const auto& n : mNL) {
      nc += n.length();
    }
    const_cast<CompositeNodeListStrategy*>(this)->mCount = nc;
  }
  return mCount;
}

const Node* CompositeNodeListStrategy::head() const
{
  return mNL.front().head();
}

NodeList CompositeNodeListStrategy::rest() const
{
  std::vector<NodeList> nl;
  auto f = mNL.front().rest();

  if (!f.empty()) {
    nl.emplace_back(std::move(f));
  }
  for (size_t i = 1; i < mNL.size(); ++i) {
    nl.emplace_back(mNL[i].clone());
  }

  if (!nl.empty()) {
    return NodeList(
      estd::make_unique<CompositeNodeListStrategy>(std::move(nl), mCount - 1));
  }

  return NodeList();
}

} // ns eyestep
