// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "style-engine.hpp"
#include "fo.hpp"
#include "scm-context.hpp"
#include "sosofo.hpp"
#include "utils.hpp"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <string>


namespace eyestep {

namespace fs = filesystem;

namespace {
  std::unique_ptr<ISchemeContext> setup_scheme_context(const std::string& prefix_path) {
    auto ctx = create_scheme_context();

    auto pfx_paths = utils::split_paths(prefix_path);

    auto lib_paths = std::vector<fs::path>{};
    transform(begin(pfx_paths), end(pfx_paths), back_inserter(lib_paths),
              [](const fs::path& path) { return path / "lib"; });

    ctx->initialize(lib_paths);

    auto init_path = fs::path("textbook") / "init.scm";
    if (!ctx->load_module_file(init_path)) {
      std::cerr << "Could not read " << init_path.string() << std::endl;
      return nullptr;
    }

    return ctx;
  }

} // namespace


StyleEngine::StyleEngine(const std::string& prefix_path, const std::string& backend_id,
                         bool verbose)
  : _backend_id(backend_id) {
  _ctx = setup_scheme_context(prefix_path);

  if (_ctx) {
    _ctx->define_variable("%textbook-prefix-paths%", prefix_path);
    _ctx->define_variable("%verbose%?", verbose);
  }
}


bool StyleEngine::load_style(const filesystem::path& path) {
  if (!_ctx) {
    return false;
  }

  _ctx->define_variable("%style-path%", path.string());
  _ctx->define_variable("%backend%", _backend_id);

  if (!_ctx->load_script(path)) {
    std::cerr << "Could not read " << path.string() << std::endl;
    return false;
  }

  return true;
}


std::unique_ptr<Sosofo> StyleEngine::process_node(const Node* root) {
  if (_ctx) {
    auto x = _ctx->process_root_node(root);
    return x;
  }

  return {};
}


void StyleEngine::define_variables(const std::vector<std::string>& defs) {
  if (!_ctx)
    return;

  for (const auto& def : defs) {
    const auto parts = utils::split(def, "=");

    auto value = parts.size() == 2 ? parts[1] : estd::optional<std::string>{};

    if (!_ctx->set_variable(parts[0], value)) {
      std::cerr << "Failed to set variable " << parts[0] << std::endl;
    }
  }
}


void StyleEngine::set_property_lookup(const fo::IProperties* pl) {
  if (_ctx) {
    _ctx->set_property_lookup(pl);
  }
}

} // namespace eyestep
