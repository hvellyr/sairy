// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodes.hpp"

#include <string>


namespace eyestep {

class Grove;

enum class TraverseRecursion {
  k_break,
  k_continue,
  k_recurse,
};

using TraverseNodeVisitor =
  std::function<TraverseRecursion(const Node*, int depth)>;

TraverseRecursion node_traverse(const Node* root,
                                const TraverseNodeVisitor& functor,
                                int depth = 0);

void serialize(std::ostream& os, const Node* nd, bool pretty_printing = true,
               int depth = 0);


std::string node_data(const Node* nd);

ConstNodes elements_with_id(const Grove* grove, const std::string& id);

} // ns eyestep
