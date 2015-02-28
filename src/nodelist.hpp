// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "nodes.hpp"

#include <vector>


namespace eyestep {

class NodeList {
public:
  enum Kind {
    kEmpty,
    kChildren,
    kSiblings, // all siblings, incl. node
    kPreced,   // all preceding siblings
    kFollow,   // all following siblings
    kAncestors,
    kDescendants,
  };

  NodeList();
  NodeList(const Node* nd, Kind kind);

  bool empty() const;
  int length() const;
  const Node* head() const;
  NodeList rest() const;

private:
  NodeList(const Node* nd, const Node* parent, Kind kind, int start, int end, int count,
           std::vector<int> stack);

  const Node* mNode;
  const Node* mParent;
  Kind mKind;
  int mStart;
  int mEnd;
  int mCount;
  std::vector<int> mStack;
};

} // ns eyestep
