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
    virtual ~INodeListStrategy() {}

    virtual std::unique_ptr<INodeListStrategy> clone() const = 0;
    virtual bool empty() const = 0;
    virtual int length() const = 0;
    virtual const Node* head() const = 0;
    virtual NodeList rest() const = 0;
  };

  class NodesNodeListStrategy : public INodeListStrategy {
  public:
    NodesNodeListStrategy(const std::vector<const Node*>& nodes, int start);

    std::unique_ptr<INodeListStrategy> clone() const override;
    bool empty() const override;
    int length() const override;
    const Node* head() const override;
    NodeList rest() const override;

    std::vector<const Node*> mNodes;
    int mStart;
  };

  class SiblingsNodeListStrategy : public INodeListStrategy {
  public:
    SiblingsNodeListStrategy(const Node* node, int start, int end);

    std::unique_ptr<INodeListStrategy> clone() const override;
    bool empty() const override;
    int length() const override;
    const Node* head() const override;
    NodeList rest() const override;

    const Node* mNode;
    int mStart;
    int mEnd;
  };

  class AncestorsNodeListStrategy : public INodeListStrategy {
  public:
    AncestorsNodeListStrategy(const Node* parent, int count);

    std::unique_ptr<INodeListStrategy> clone() const override;
    bool empty() const override;
    int length() const override;
    const Node* head() const override;
    NodeList rest() const override;

    const Node* mNode;
    int mCount;
  };

  class DescendantsNodeListStrategy : public INodeListStrategy {
  public:
    DescendantsNodeListStrategy(const Node* node, int start, int end, int count,
                                std::vector<int> stack);

    std::unique_ptr<INodeListStrategy> clone() const override;
    bool empty() const override;
    int length() const override;
    const Node* head() const override;
    NodeList rest() const override;

    const Node* mNode;
    int mStart;
    int mEnd;
    int mCount;
    std::vector<int> mStack;
  };

  class CompositeNodeListStrategy : public INodeListStrategy {
  public:
    CompositeNodeListStrategy(std::vector<NodeList> nl, int count);

    std::unique_ptr<INodeListStrategy> clone() const override;
    bool empty() const override;
    int length() const override;
    const Node* head() const override;
    NodeList rest() const override;

    std::vector<NodeList> mNL;
    int mCount;
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
  NodeList(const std::vector<NodeList>& nl);
  NodeList(const std::vector<const Node*>& nodes);

  // internal
  NodeList(std::unique_ptr<detail::INodeListStrategy> strategy);

  NodeList clone() const;
  bool empty() const;
  int length() const;
  const Node* head() const;
  NodeList rest() const;

private:
  friend class detail::DescendantsNodeListStrategy;

  std::unique_ptr<detail::INodeListStrategy> mStrategy;
};

} // ns eyestep
