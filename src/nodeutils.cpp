// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodeutils.hpp"
#include "nodes.hpp"
#include "nodelist.hpp"
#include "nodeclass.hpp"

#include <iostream>
#include <string>


namespace eyestep {

//------------------------------------------------------------------------------

TraverseRecursion node_traverse(const Node* root,
                                const TraverseNodeVisitor& functor, int depth)
{
  TraverseRecursion rec = functor(root, depth);

  if (rec == TraverseRecursion::k_recurse) {
    const Nodes& nodes = root->property<Nodes>(CommonProps::k_children);

    for (const auto& nd : nodes) {
      TraverseRecursion rec2 = node_traverse(nd, functor, depth + 1);
      if (rec2 == TraverseRecursion::k_break) {
        return rec2;
      }
    }
  }

  return rec;
}


namespace {
  class SerializeVisitor : public boost::static_visitor<> {
    std::ostream& _os;
    int _depth;

  public:
    SerializeVisitor(std::ostream& os, int depth) : _os(os), _depth(depth) {}

    void operator()(const Undefined&) {}

    void operator()(const int& val) { _os << val; }

    void operator()(const std::string& val) { _os << val; }

    void operator()(const Node* nd)
    {
      _os << std::endl;
      serialize(_os, nd, _depth);
      _os << std::string((_depth - 1) * 2, ' ');
    }

    void operator()(const Nodes& nl)
    {
      if (!nl.empty()) {
        _os << std::endl;
        for (auto* nd : nl) {
          serialize(_os, nd, _depth);
        }
        _os << std::string((_depth - 1) * 2, ' ');
      }
    }
  };

} // ns anon


void serialize(std::ostream& os, const Node* nd, int in_depth)
{
  int last_depth = in_depth - 1;

  auto close_node = [&os](const Node* n, int lastd, int depth) {
    for (int i = lastd; i >= depth; --i) {
      os << std::string(i * 2, ' ') << "</" << n->classname() << ">"
         << std::endl;
    }
  };

  node_traverse(nd, [&os, &last_depth, &close_node](const Node* nd, int depth) {
    close_node(nd, last_depth, depth);
    last_depth = depth;

    os << std::string(depth * 2, ' ') << "<" << nd->classname();
    if (nd->node_class() == element_class_definition()) {
      os << " gi='" << nd->gi() << "'";
    }
    os << ">" << std::endl;
    for (const auto& prop : nd->properties()) {
      if (prop.first != CommonProps::k_children &&
          prop.first != CommonProps::k_gi &&
          prop.first != CommonProps::k_parent) {
        os << std::string((depth + 1) * 2, ' ') << "<prop nm='" << prop.first
           << "'>";
        SerializeVisitor visitor(os, depth + 2);
        boost::apply_visitor(visitor, prop.second);
        os << "</prop>" << std::endl;
      }
    }
    return TraverseRecursion::k_recurse;
  }, in_depth);

  close_node(nd, last_depth, in_depth);
}

} // ns eyestep
