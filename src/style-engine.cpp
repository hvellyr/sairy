// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "scm-context.hpp"
#include "sosofo.hpp"
#include "style-engine.hpp"

#include <boost/range/adaptor/transformed.hpp>

#include <cassert>
#include <iostream>


namespace eyestep {

namespace fs = boost::filesystem;

namespace {
  std::unique_ptr<ISchemeContext>
  setup_scheme_context(const std::vector<fs::path>& prefix_paths)
  {
    auto ctx = createSchemeContext();

    auto paths = boost::copy_range<std::vector<fs::path>>(
        prefix_paths | boost::adaptors::transformed(
                           [](const fs::path& path) { return path / "lib"; }));

    ctx->initialize(paths);

    auto init_path = fs::path("sairy") / "init.scm";
    if (!ctx->loadModuleFile(init_path)) {
      std::cerr << "Could not read " << init_path.string() << std::endl;
      return nullptr;
    }

    return std::move(ctx);
  }

} // ns anon


StyleEngine::StyleEngine(const std::vector<fs::path>& prefix_paths)
{
  mCtx = setup_scheme_context(prefix_paths);
}


bool StyleEngine::loadStyle(const boost::filesystem::path& path)
{
  assert(mCtx);

  if (!mCtx->loadScript(path)) {
    std::cerr << "Could not read " << path.string() << std::endl;
    return false;
  }

  return true;
}


std::unique_ptr<Sosofo> StyleEngine::processNode(const Node* root)
{
  assert(mCtx);

  return std::move(mCtx->processRootNode(root));
}

} // ns eyestep
