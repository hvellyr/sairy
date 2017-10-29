// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fspp/filesystem.hpp"

#include <memory>
#include <string>
#include <vector>

namespace eyestep {

class Node;
class Sosofo;
class ISchemeContext;


class StyleEngine
{
  std::unique_ptr<ISchemeContext> _ctx;
  std::string _backend_id;

public:
  StyleEngine(const std::string& prefix_path, const std::string& backend_id);

  bool load_style(const filesystem::path& path);
  void define_variables(const std::vector<std::string>& defs);

  std::unique_ptr<Sosofo> process_node(const Node* node);
};

} // ns eyestep
