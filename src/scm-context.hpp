// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fspp/filesystem.hpp"

#include <memory>
#include <vector>


namespace eyestep {

class Node;
class Sosofo;

class ISchemeContext {
public:
  virtual ~ISchemeContext(){};

  virtual void initialize(const std::vector<filesystem::path>& module_paths) = 0;
  virtual bool load_module_file(const filesystem::path& script_file) = 0;
  virtual bool load_script(const filesystem::path& script_file) = 0;

  virtual void define_variable(const std::string& name, const std::string& value) = 0;

  virtual std::unique_ptr<Sosofo> process_root_node(const Node* root_node) = 0;
};

std::unique_ptr<ISchemeContext> create_scheme_context();

} // ns eyestep
