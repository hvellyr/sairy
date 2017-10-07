// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodelist.hpp"
#include "estd/memory.hpp"

#include <algorithm>
#include <cassert>
#include <memory>
#include <numeric>
#include <utility>


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

NodeList::NodeList(const Node* nd, Kind kind) {
  using namespace std;

  assert(nd);

  if (kind == k_children) {
    int end = int(nd->property<Nodes>(CommonProps::k_children).size());
    if (end > 0)
      _strategy = estd::make_unique<SiblingsNodeListStrategy>(nd, 0, end);
  }
  else if (kind == k_siblings) {
    const auto* parent = nd->parent();
    int end = int(parent->property<Nodes>(CommonProps::k_children).size());

    if (end > 0)
      _strategy = estd::make_unique<SiblingsNodeListStrategy>(parent, 0, end);
  }
  else if (kind == k_preced) {
    const auto* parent = nd->parent();
    const auto& nl = parent->property<Nodes>(CommonProps::k_children);
    auto i_find = find_if(begin(nl), end(nl), [&](const Node* n) { return nd == n; });
    int end = distance(begin(nl), i_find);

    if (end > 0)
      _strategy = estd::make_unique<SiblingsNodeListStrategy>(parent, 0, end);
  }
  else if (kind == k_follow) {
    const auto* parent = nd->parent();
    const auto& nl = parent->property<Nodes>(CommonProps::k_children);
    auto i_find = find_if(begin(nl), end(nl), [&](const Node* n) { return nd == n; });
    int start = distance(begin(nl), i_find) + 1;
    int end = int(nl.size());

    if (start < end)
      _strategy = estd::make_unique<SiblingsNodeListStrategy>(parent, start, end);
  }
  else if (kind == k_ancestors) {
    _strategy = estd::make_unique<AncestorsNodeListStrategy>(nd->parent(), -1);
  }
  else if (kind == k_descendants) {
    int end = int(nd->property<Nodes>(CommonProps::k_children).size());

    if (end > 0)
      _strategy =
        estd::make_unique<DescendantsNodeListStrategy>(nd, 0, end, -1, vector<int>{});
  }
}


NodeList::NodeList(const std::vector<NodeList>& nl) {
  auto result = std::vector<NodeList>{};

  for (const auto& n : nl) {
    if (!n.empty())
      result.emplace_back(n.clone());
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
    return {estd::make_unique<SiblingsNodeListStrategy>(_node, _start + 1, _end)};
  }
  return {};
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
    const auto* p = _node;
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
    return {estd::make_unique<AncestorsNodeListStrategy>(p, _count - 1)};
  }
  return {};
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
  return _count < 0 ? length() == 0 : _count == 0;
}


int DescendantsNodeListStrategy::length() const {
  if (_count < 0) {
    auto nc = 1;
    auto nl = rest();
    while (nl._strategy.get()) {
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
  using namespace std;

  if (_start < _end) {
    const auto* newnode = _node->property<Nodes>(CommonProps::k_children)[_start];

    const auto& children = newnode->property<Nodes>(CommonProps::k_children);
    if (children.size() > 0) {
      auto stack = vector<int>(_stack);
      stack.emplace_back(_start + 1);

      return {
        estd::make_unique<DescendantsNodeListStrategy>(newnode, 0,
                                                       int(newnode
                                                             ->property<Nodes>(
                                                               CommonProps::k_children)
                                                             .size()),
                                                       _count - 1, move(stack))};
    }
  }

  if (_start + 1 < _end) {
    return {estd::make_unique<DescendantsNodeListStrategy>(_node, _start + 1, _end,
                                                           _count - 1, _stack)};
  }

  if (_stack.size() > 0) {
    auto* p = _node->parent();
    auto i_last = prev(_stack.end());

    while (p && i_last >= _stack.begin()) {
      auto end = int(p->property<Nodes>(CommonProps::k_children).size());
      auto start = *i_last;
      if (start < end) {
        auto stack = vector<int>(_stack.begin(), i_last);
        return {estd::make_unique<DescendantsNodeListStrategy>(p, start, end, _count - 1,
                                                               move(stack))};
      }

      p = p->parent();
      --i_last;
    }
  }

  return {};
}


//------------------------------------------------------------------------------

CompositeNodeListStrategy::CompositeNodeListStrategy(std::vector<NodeList> nl, int count)
  : _nl(std::move(nl))
  , _count(count) {}


std::unique_ptr<INodeListStrategy> CompositeNodeListStrategy::clone() const {
  auto nl = std::vector<NodeList>{};
  for (const auto& n : _nl) {
    nl.emplace_back(n.clone());
  }
  return estd::make_unique<CompositeNodeListStrategy>(std::move(nl), _count);
}


bool CompositeNodeListStrategy::empty() const {
  return _nl.empty();
}


int CompositeNodeListStrategy::length() const {
  using namespace std;

  if (_count < 0) {
    auto nc = accumulate(begin(_nl), end(_nl), 0, [](const int count, const NodeList& n) {
      return count + n.length();
    });
    const_cast<CompositeNodeListStrategy*>(this)->_count = nc;
  }
  return _count;
}


const Node* CompositeNodeListStrategy::head() const {
  return _nl.front().head();
}


NodeList CompositeNodeListStrategy::rest() const {
  using namespace std;

  auto nl = vector<NodeList>{};
  auto f = _nl.front().rest();

  if (!f.empty()) {
    nl.emplace_back(move(f));
  }
  for (auto i = 1u; i < _nl.size(); ++i) {
    nl.emplace_back(_nl[i].clone());
  }

  if (!nl.empty()) {
    return {estd::make_unique<CompositeNodeListStrategy>(move(nl), _count - 1)};
  }

  return {};
}

} // ns eyestep
