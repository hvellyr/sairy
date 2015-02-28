// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodelist.hpp"
#include "estd/memory.hpp"

#include <cassert>


namespace eyestep {

//------------------------------------------------------------------------------

NodeList::NodeList() : NodeList(nullptr, nullptr, kEmpty, 0, 0, 0, {})
{
}


NodeList::NodeList(const Node* nd, Kind kind)
    : NodeList(nd, nullptr, kind, 0, -1, -1, {})
{
  assert(mNode);

  switch (mKind) {
  case kEmpty:
    break;
  case kChildren:
    mEnd = int(mNode->property<Nodes>(CommonProps::kChildren).size());
    mKind = mStart >= mEnd ? kEmpty : mKind;
    break;
  case kSiblings:
    mParent = mNode->parent();
    mEnd = int(mParent->property<Nodes>(CommonProps::kChildren).size());
    mKind = mStart >= mEnd ? kEmpty : mKind;
    break;
  case kPreced:
    mParent = mNode->parent();
    {
      const Nodes& nl = mParent->property<Nodes>(CommonProps::kChildren);
      auto i_find = std::find_if(nl.begin(), nl.end(),
                                 [&](const Node* n) { return nd == n; });
      mEnd = std::distance(nl.begin(), i_find);
      mKind = mStart >= mEnd ? kEmpty : mKind;
    }
    break;
  case kFollow:
    mParent = mNode->parent();
    {
      const Nodes& nl = mParent->property<Nodes>(CommonProps::kChildren);
      auto i_find = std::find_if(nl.begin(), nl.end(),
                                 [&](const Node* n) { return nd == n; });
      mStart = std::distance(nl.begin(), i_find) + 1;
      mEnd = int(nl.size());
      mKind = mStart >= mEnd ? kEmpty : mKind;
    }
    break;
  case kAncestors:
    mNode = mNode->parent();
    mCount = -1;
    break;
  case kDescendants:
    mStart = 0;
    mParent = mNode;
    mEnd = int(mNode->property<Nodes>(CommonProps::kChildren).size());
    mKind = mStart >= mEnd ? kEmpty : mKind;
    break;
  }
}


NodeList::NodeList(const Node* nd, const Node* parent, Kind kind, int start,
                   int end, int count, std::vector<int> stack)
    : mNode(nd), mParent(parent), mKind(kind), mStart(start), mEnd(end),
      mCount(count), mStack(std::move(stack))
{
  assert(mNode || mKind == kEmpty);
}


bool NodeList::empty() const
{
  switch (mKind) {
  case kEmpty:
    return true;
  case kChildren:
  case kSiblings:
  case kPreced:
  case kFollow:
    return mEnd - mStart == 0;
  case kAncestors:
    return mCount == 0 || mNode == nullptr;
  case kDescendants:
    if (mCount < 0) {
      return mKind == kEmpty;
    }
    return mCount == 0;
  }
  return true;
}


int NodeList::length() const
{
  switch (mKind) {
  case kEmpty:
    return 0;
  case kChildren:
  case kSiblings:
  case kPreced:
  case kFollow:
    return mEnd - mStart;
  case kAncestors:
    if (mCount < 0) {
      const Node* p = mNode;
      int nc = 0;
      while (p) {
        nc++;
        p = p->parent();
      }
      const_cast<NodeList*>(this)->mCount = nc;
    }
    return mCount;
  case kDescendants:
    if (mCount < 0) {
      int nc = 1;
      NodeList nl = rest();
      while (nl.mKind != kEmpty) {
        nl = nl.rest();
        nc++;
      }

      const_cast<NodeList*>(this)->mCount = nc;
    }
    return mCount;
  }
  return 0;
}


const Node* NodeList::head() const
{
  switch (mKind) {
  case kEmpty:
    return nullptr;
  case kChildren:
    assert(mStart < mEnd);
    return mNode->property<Nodes>(CommonProps::kChildren)[mStart];
  case kSiblings:
  case kPreced:
  case kFollow:
    assert(mStart < mEnd);
    return mParent->property<Nodes>(CommonProps::kChildren)[mStart];
  case kAncestors:
    return mNode;
  case kDescendants:
    assert(mStart < mEnd);
    return mNode->property<Nodes>(CommonProps::kChildren)[mStart];
  }

  return nullptr;
}


NodeList NodeList::rest() const
{
  switch (mKind) {
  case kEmpty:
    return *this;
  case kChildren:
  case kSiblings:
  case kPreced:
  case kFollow:
    if (mStart + 1 < mEnd) {
      return NodeList(mNode, mParent, mKind, mStart + 1, mEnd, mCount, mStack);
    }
    break;
  case kAncestors:
    if (Node* p = mNode->parent()) {
      return NodeList(p, nullptr, kAncestors, 0, 0, mCount - 1, mStack);
    }
    break;
  case kDescendants:
    if (mStart < mEnd) {
      Node* newNode = mNode->property<Nodes>(CommonProps::kChildren)[mStart];

      const Nodes& children = newNode->property<Nodes>(CommonProps::kChildren);
      if (children.size() > 0) {
        std::vector<int> stack(mStack);
        stack.emplace_back(mStart + 1);
        return NodeList(newNode, nullptr, kDescendants, 0,
                        int(newNode->property<Nodes>(CommonProps::kChildren)
                                .size()),
                        mCount - 1, std::move(stack));
      }
    }
    if (mStart + 1 < mEnd) {
      return NodeList(mNode, nullptr, kDescendants, mStart + 1, mEnd,
                      mCount - 1, mStack);
    }
    if (mStack.size() > 0) {
      Node* p = mNode->parent();
      auto i_last = std::prev(mStack.end());

      while (p && i_last >= mStack.begin()) {
        auto end = int(p->property<Nodes>(CommonProps::kChildren).size());
        auto start = *i_last;
        if (start < end) {
          std::vector<int> stack(mStack.begin(), i_last);
          return NodeList(p, nullptr, kDescendants, start, end, mCount - 1,
                          std::move(stack));
        }

        p = p->parent();
        --i_last;
      }
    }
    break;
  }

  return NodeList();
}

} // ns eyestep
