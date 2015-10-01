// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodeutils.hpp"
#include "nodes.hpp"
#include "nodelist.hpp"
#include "nodeclass.hpp"

#include <iostream>
#include <sstream>
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
  std::ostream& escape(std::ostream& os, const std::string& str)
  {
    for (const auto c : str) {
      switch (c) {
      case '\n':
        os << "\\n";
        break;
      case '\r':
        os << "\\r";
        break;
      case '\t':
        os << "\\t";
        break;
      case '"':
        os << "\\\"";
        break;
      default:
        os << c;
      }
    }

    return os;
  }

  class JsPrinter {
    std::ostream& _os;
    bool _do_indent;

  public:
    JsPrinter(std::ostream& os, bool do_indent) : _os(os), _do_indent(do_indent)
    {
    }

    const JsPrinter& newln() const
    {
      if (_do_indent) {
        _os << std::endl;
      }
      return *this;
    }

    const JsPrinter& indent(int depth, int ofs) const
    {
      if (_do_indent) {
        _os << std::string(depth * 4 + ofs * 2, ' ');
      }
      return *this;
    }

    const JsPrinter& sep() const
    {
      _os << ",";
      return newln();
    }

    const JsPrinter& open_obj() const
    {
      _os << "{";
      if (_do_indent) {
        _os << " ";
      }
      return *this;
    }

    const JsPrinter& close_obj() const
    {
      _os << "}";
      return *this;
    }

    const JsPrinter& open_array() const
    {
      _os << "[";
      return newln();
    }

    const JsPrinter& close_array() const
    {
      _os << "]";
      return *this;
    }

    const JsPrinter& empty_array() const
    {
      _os << "[]";
      return *this;
    }

    const JsPrinter& print_attrnm(const std::string& nm) const
    {
      _os << "\"" << nm << "\":";
      if (_do_indent) {
        _os << " ";
      }
      return *this;
    }

    const JsPrinter& print_value(const std::string& val) const
    {
      _os << "\"";
      escape(_os, val) << "\"";
      return *this;
    }

    const JsPrinter& print_value(int val) const
    {
      _os << val;
      return *this;
    }
  };

  void serialize(const JsPrinter& pp, const Node* nd, int depth);

  class SerializeVisitor : public boost::static_visitor<> {
    JsPrinter _pp;
    int _depth;

  public:
    SerializeVisitor(const JsPrinter& pp, int depth) : _pp(pp), _depth(depth) {}

    void operator()(const Undefined&) {}

    void operator()(const int& val) { _pp.print_value(val); }

    void operator()(const std::string& val) { _pp.print_value(val); }

    void operator()(const Node* nd)
    {
      _pp.newln();
      serialize(_pp, nd, _depth);
    }

    void operator()(const Nodes& nl)
    {
      if (!nl.empty()) {
        _pp.open_array();
        bool first = true;
        for (auto* nd : nl) {
          if (!first) {
            _pp.sep();
          }
          else {
            first = false;
          }
          serialize(_pp, nd, _depth);
        }
        _pp.newln().indent(_depth, -1).close_array();
      }
      else {
        _pp.empty_array();
      }
    }
  };

  void serialize(const JsPrinter& pp, const Node* nd, int depth)
  {
    pp.indent(depth, 0).open_obj().print_attrnm("type").print_value(
      nd->classname());

    if (nd->node_class() == element_class_definition()) {
      pp.sep().indent(depth, 1).print_attrnm("gi").print_value(nd->gi());
    }

    for (const auto& prop : nd->properties()) {
      if (prop.first != CommonProps::k_gi &&
          prop.first != CommonProps::k_parent) {
        pp.sep().indent(depth, 1).print_attrnm(prop.first);

        SerializeVisitor visitor(pp, depth + 1);
        boost::apply_visitor(visitor, prop.second);
      }
    }

    pp.newln().indent(depth, 0).close_obj();

    if (depth == 0) {
      pp.newln();
    }
  }

} // ns anon


void serialize(std::ostream& os, const Node* nd, bool pretty_printing,
               int depth)
{
  auto pp = JsPrinter{os, pretty_printing};
  serialize(pp, nd, depth);
}


//------------------------------------------------------------------------------

std::string node_data(const Node* base_nd)
{
  std::stringstream ss;

  node_traverse(base_nd, [&ss](const Node* nd, int /*depth*/) {
    ss << nd->property<std::string>(CommonProps::k_data);
    return TraverseRecursion::k_recurse;
  }, 0);

  return ss.str();
}


ConstNodes elements_with_id(const Grove* grove, const std::string& id)
{
  ConstNodes nl_result;

  // if grove would keep up a mapping from ids to nodes this could be easily
  // optimized
  NodeList nl(grove->root_node(), NodeList::k_descendants);
  while (!nl.empty()) {
    const auto* first = nl.head();
    if (first->property<std::string>(CommonProps::k_id) == id) {
      nl_result.push_back(first);
    }

    nl = nl.rest();
  }

  return nl_result;
}


Node* desc_element(const Node* nd)
{
  const Nodes& nodes = nd->property<Nodes>(CommonProps::k_children);
  for (auto& child : nodes) {
    if (child->gi() == CommonProps::k_desc) {
      return child;
    }
  }

  return nullptr;
}

} // ns eyestep
