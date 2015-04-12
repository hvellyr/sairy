// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "scm-context.hpp"
#include "sosofo.hpp"
#include "style-engine.hpp"
#include "utils.hpp"

#include <boost/range/adaptor/transformed.hpp>

#include <cassert>
#include <iostream>
#include <string>


namespace eyestep {

namespace fs = boost::filesystem;

namespace {
  std::unique_ptr<ISchemeContext>
  setup_scheme_context(const std::string& prefix_path)
  {
    auto ctx = create_scheme_context();

    auto paths = boost::copy_range<std::vector<fs::path>>(
      eyestep::utils::split_paths(prefix_path)
      | boost::adaptors::transformed(
                       [](const fs::path& path) { return path / "lib"; }));

    ctx->initialize(paths);

    auto init_path = fs::path("sairy") / "init.scm";
    if (!ctx->load_module_file(init_path)) {
      std::cerr << "Could not read " << init_path.string() << std::endl;
      return nullptr;
    }

    return std::move(ctx);
  }

} // ns anon


StyleEngine::StyleEngine(const std::string& prefix_path,
                         const std::string& backend_id)
  : _backend_id(backend_id)
{
  _ctx = setup_scheme_context(prefix_path);
  _ctx->define_variable("%sairy-prefix-paths%", prefix_path);
}


bool StyleEngine::load_style(const boost::filesystem::path& path)
{
  assert(_ctx);

  _ctx->define_variable("%style-path%", path.string());
  _ctx->define_variable("%backend%", _backend_id);

  if (!_ctx->load_script(path)) {
    std::cerr << "Could not read " << path.string() << std::endl;
    return false;
  }

  return true;
}


std::unique_ptr<Sosofo> StyleEngine::process_node(const Node* root)
{
  assert(_ctx);

  return std::move(_ctx->process_root_node(root));
}

} // ns eyestep
