// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodelist.hpp"
#include "estd/memory.hpp"

#include <algorithm>
#include <cassert>
#include <memory>


namespace eyestep {

using detail::INodeListStrategy;
using detail::SiblingsNodeListStrategy;
using detail::AncestorsNodeListStrategy;
using detail::DescendantsNodeListStrategy;
using detail::CompositeNodeListStrategy;


//------------------------------------------------------------------------------

namespace detail {

  template <typename T>
  class NodesNodeListStrategy : public INodeListStrategy
  {
    T _nodes;
    int _start;

  public:
    NodesNodeListStrategy(const T& nodes, int start)
      : _nodes(nodes)
      , _start(start) {}

    std::unique_ptr<INodeListStrategy> clone() const override {
      return estd::make_unique<NodesNodeListStrategy>(_nodes, _start);
    }

    bool empty() const override {
      return int(_nodes.size()) - _start == 0;
    }

    int length() const override {
      return int(_nodes.size()) - _start;
    }

    const Node* head() const override {
      assert(_start < int(_nodes.size()));
      return _nodes[_start];
    }

    NodeList rest() const override {
      if (_start + 1 < int(_nodes.size())) {
        return NodeList(
          std::move(estd::make_unique<NodesNodeListStrategy>(_nodes, _start + 1)));
      }
      return NodeList();
    }
  };
} // detail ns

using detail::NodesNodeListStrategy;

//------------------------------------------------------------------------------

NodeList::NodeList() {}


NodeList::NodeList(const Node* nd, Kind kind) {
  assert(nd);

  if (kind == k_children) {
    int end = int(nd->property<Nodes>(CommonProps::k_children).size());
    if (end > 0) {
      _strategy = estd::make_unique<SiblingsNodeListStrategy>(nd, 0, end);
    }
  }
  else if (kind == k_siblings) {
    const auto* parent = nd->parent();
    int end = int(parent->property<Nodes>(CommonProps::k_children).size());
    if (end > 0) {
      _strategy = estd::make_unique<SiblingsNodeListStrategy>(parent, 0, end);
    }
  }
  else if (kind == k_preced) {
    const auto* parent = nd->parent();
    const Nodes& nl = parent->property<Nodes>(CommonProps::k_children);
    auto i_find =
      std::find_if(nl.begin(), nl.end(), [&](const Node* n) { return nd == n; });
    int end = std::distance(nl.begin(), i_find);
    if (end > 0) {
      _strategy = estd::make_unique<SiblingsNodeListStrategy>(parent, 0, end);
    }
  }
  else if (kind == k_follow) {
    const auto* parent = nd->parent();
    const Nodes& nl = parent->property<Nodes>(CommonProps::k_children);
    auto i_find =
      std::find_if(nl.begin(), nl.end(), [&](const Node* n) { return nd == n; });
    int start = std::distance(nl.begin(), i_find) + 1;
    int end = int(nl.size());

    if (start < end) {
      _strategy = estd::make_unique<SiblingsNodeListStrategy>(parent, start, end);
    }
  }
  else if (kind == k_ancestors) {
    _strategy = estd::make_unique<AncestorsNodeListStrategy>(nd->parent(), -1);
  }
  else if (kind == k_descendants) {
    int end = int(nd->property<Nodes>(CommonProps::k_children).size());
    if (end > 0) {
      _strategy = estd::make_unique<DescendantsNodeListStrategy>(nd, 0, end, -1,
                                                                 std::vector<int>{});
    }
  }
}


NodeList::NodeList(const std::vector<NodeList>& nl) {
  std::vector<NodeList> result;
  for (const auto& n : nl) {
    if (!n.empty()) {
      result.emplace_back(n.clone());
    }
  }
  _strategy = estd::make_unique<detail::CompositeNodeListStrategy>(std::move(result), -1);
}


NodeList::NodeList(std::unique_ptr<INodeListStrategy> strategy)
  : _strategy(std::move(strategy)) {}


NodeList::NodeList(const Nodes& nodes)
  : _strategy(estd::make_unique<NodesNodeListStrategy<Nodes>>(nodes, 0)) {}


NodeList::NodeList(const ConstNodes& nodes)
  : _strategy(estd::make_unique<NodesNodeListStrategy<ConstNodes>>(nodes, 0)) {}


NodeList NodeList::clone() const {
  return _strategy ? NodeList(_strategy->clone()) : NodeList();
}


bool NodeList::empty() const {
  return _strategy ? _strategy->empty() : true;
}


int NodeList::length() const {
  return _strategy ? _strategy->length() : 0;
}


const Node* NodeList::head() const {
  return _strategy ? _strategy->head() : nullptr;
}


NodeList NodeList::rest() const {
  return _strategy ? _strategy->rest() : NodeList();
}


//------------------------------------------------------------------------------

SiblingsNodeListStrategy::SiblingsNodeListStrategy(const Node* node, int start, int end)
  : _node(node)
  , _start(start)
  , _end(end) {}


std::unique_ptr<INodeListStrategy> SiblingsNodeListStrategy::clone() const {
  return estd::make_unique<SiblingsNodeListStrategy>(_node, _start, _end);
}


bool SiblingsNodeListStrategy::empty() const {
  return _end - _start == 0;
}


int SiblingsNodeListStrategy::length() const {
  return _end - _start;
}


const Node* SiblingsNodeListStrategy::head() const {
  assert(_start < _end);
  return _node->property<Nodes>(CommonProps::k_children)[_start];
}


NodeList SiblingsNodeListStrategy::rest() const {
  if (_start + 1 < _end) {
    return NodeList(estd::make_unique<SiblingsNodeListStrategy>(_node, _start + 1, _end));
  }
  return NodeList();
}


//------------------------------------------------------------------------------

AncestorsNodeListStrategy::AncestorsNodeListStrategy(const Node* parent, int count)
  : _node(parent)
  , _count(count) {}


std::unique_ptr<INodeListStrategy> AncestorsNodeListStrategy::clone() const {
  return estd::make_unique<AncestorsNodeListStrategy>(_node, _count);
}


bool AncestorsNodeListStrategy::empty() const {
  return _count == 0 || _node == nullptr;
}


int AncestorsNodeListStrategy::length() const {
  if (_count < 0) {
    const Node* p = _node;
    int nc = 0;
    while (p) {
      nc++;
      p = p->parent();
    }
    const_cast<AncestorsNodeListStrategy*>(this)->_count = nc;
  }
  return _count;
}


const Node* AncestorsNodeListStrategy::head() const {
  return _node;
}


NodeList AncestorsNodeListStrategy::rest() const {
  if (Node* p = _node->parent()) {
    return NodeList(estd::make_unique<AncestorsNodeListStrategy>(p, _count - 1));
  }
  return NodeList();
}


//------------------------------------------------------------------------------

DescendantsNodeListStrategy::DescendantsNodeListStrategy(const Node* node, int start,
                                                         int end, int count,
                                                         std::vector<int> stack)
  : _node(node)
  , _start(start)
  , _end(end)
  , _count(count)
  , _stack(std::move(stack)) {}


std::unique_ptr<INodeListStrategy> DescendantsNodeListStrategy::clone() const {
  return estd::make_unique<DescendantsNodeListStrategy>(_node, _start, _end, _count,
                                                        _stack);
}


bool DescendantsNodeListStrategy::empty() const {
  if (_count < 0) {
    return length() == 0;
  }
  return _count == 0;
}


int DescendantsNodeListStrategy::length() const {
  if (_count < 0) {
    int nc = 1;
    NodeList nl = rest();
    while (nl._strategy.get() != nullptr) {
      nl = nl.rest();
      nc++;
    }

    const_cast<DescendantsNodeListStrategy*>(this)->_count = nc;
  }
  return _count;
}


const Node* DescendantsNodeListStrategy::head() const {
  assert(_start < _end);
  return _node->property<Nodes>(CommonProps::k_children)[_start];
}


NodeList DescendantsNodeListStrategy::rest() const {
  if (_start < _end) {
    const Node* newnode = _node->property<Nodes>(CommonProps::k_children)[_start];

    const Nodes& children = newnode->property<Nodes>(CommonProps::k_children);
    if (children.size() > 0) {
      std::vector<int> stack(_stack);
      stack.emplace_back(_start + 1);

      return NodeList(
        estd::make_unique<DescendantsNodeListStrategy>(newnode, 0,
                                                       int(newnode
                                                             ->property<Nodes>(
                                                               CommonProps::k_children)
                                                             .size()),
                                                       _count - 1, std::move(stack)));
    }
  }

  if (_start + 1 < _end) {
    return NodeList(estd::make_unique<DescendantsNodeListStrategy>(_node, _start + 1,
                                                                   _end, _count - 1,
                                                                   _stack));
  }

  if (_stack.size() > 0) {
    Node* p = _node->parent();
    auto i_last = std::prev(_stack.end());

    while (p && i_last >= _stack.begin()) {
      auto end = int(p->property<Nodes>(CommonProps::k_children).size());
      auto start = *i_last;
      if (start < end) {
        std::vector<int> stack(_stack.begin(), i_last);
        return NodeList(estd::make_unique<DescendantsNodeListStrategy>(p, start, end,
                                                                       _count - 1,
                                                                       std::move(stack)));
      }

      p = p->parent();
      --i_last;
    }
  }

  return NodeList();
}


//------------------------------------------------------------------------------

CompositeNodeListStrategy::CompositeNodeListStrategy(std::vector<NodeList> nl, int count)
  : _nl(std::move(nl))
  , _count(count) {}


std::unique_ptr<INodeListStrategy> CompositeNodeListStrategy::clone() const {
  std::vector<NodeList> nl;
  for (const auto& n : _nl) {
    nl.emplace_back(n.clone());
  }
  return estd::make_unique<CompositeNodeListStrategy>(std::move(nl), _count);
}


bool CompositeNodeListStrategy::empty() const {
  return _nl.empty();
}


int CompositeNodeListStrategy::length() const {
  if (_count < 0) {
    int nc = 0;
    for (const auto& n : _nl) {
      nc += n.length();
    }
    const_cast<CompositeNodeListStrategy*>(this)->_count = nc;
  }
  return _count;
}


const Node* CompositeNodeListStrategy::head() const {
  return _nl.front().head();
}


NodeList CompositeNodeListStrategy::rest() const {
  std::vector<NodeList> nl;
  auto f = _nl.front().rest();

  if (!f.empty()) {
    nl.emplace_back(std::move(f));
  }
  for (size_t i = 1; i < _nl.size(); ++i) {
    nl.emplace_back(_nl[i].clone());
  }

  if (!nl.empty()) {
    return NodeList(
      estd::make_unique<CompositeNodeListStrategy>(std::move(nl), _count - 1));
  }

  return NodeList();
}

} // ns eyestep
