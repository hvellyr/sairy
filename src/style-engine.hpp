// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fspp/filesystem.hpp"

#include <memory>
#include <string>
#include <vector>

namespace eyestep {

class ISchemeContext;
class Node;
class Sosofo;

namespace fo {
  class IProperties;
}

class StyleEngine
{
  std::unique_ptr<ISchemeContext> _ctx;
  std::string _backend_id;

public:
  StyleEngine(const std::string& prefix_path, const std::string& backend_id,
              bool verbose);

  bool load_style(const filesystem::path& path);
  void define_variables(const std::vector<std::string>& defs);
  void set_property_lookup(const fo::IProperties* pl);

  std::unique_ptr<Sosofo> process_node(const Node* node);
};

} // namespace eyestep
