// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "nodes.hpp"

#include <memory>
#include <vector>


namespace eyestep {

class NodeList;


namespace detail {
  class INodeListStrategy {
  public:
    virtual ~INodeListStrategy() { }

    virtual bool empty() const = 0;
    virtual int length() const = 0;
    virtual const Node* head() const = 0;
    virtual NodeList rest() const = 0;
  };

  class SiblingsNodeListStrategy : public INodeListStrategy  {
  public:
    SiblingsNodeListStrategy(const Node* node, int start, int end);

    bool empty() const;
    int length() const;
    const Node* head() const;
    NodeList rest() const;

    const Node* mNode;
    int mStart;
    int mEnd;
  };

  class AncestorsNodeListStrategy : public INodeListStrategy  {
  public:
    AncestorsNodeListStrategy(const Node* parent, int count);

    bool empty() const;
    int length() const;
    const Node* head() const;
    NodeList rest() const;

    const Node* mNode;
    int mCount;
  };

  class DescendantsNodeListStrategy : public INodeListStrategy  {
  public:
    DescendantsNodeListStrategy(const Node* node, int start, int end, int count,
                                std::vector<int> stack);

    bool empty() const;
    int length() const;
    const Node* head() const;
    NodeList rest() const;

    const Node* mNode;
    int mStart;
    int mEnd;
    int mCount;
    std::vector<int> mStack;
  };

} // detail ns


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

  // internal
  NodeList(std::unique_ptr<detail::INodeListStrategy> strategy);

  bool empty() const;
  int length() const;
  const Node* head() const;
  NodeList rest() const;

private:
  friend class detail::DescendantsNodeListStrategy;

  std::unique_ptr<detail::INodeListStrategy> mStrategy;
};

} // ns eyestep
