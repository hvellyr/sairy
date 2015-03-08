// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/filesystem.hpp>
#include <memory>
#include <string>
#include <vector>

namespace eyestep {

class Node;
class Sosofo;
class ISchemeContext;


class StyleEngine {
  std::unique_ptr<ISchemeContext> mCtx;
  std::string mBackendId;

public:
  StyleEngine(const std::vector<boost::filesystem::path>& prefix_paths,
              const std::string& backendId);

  bool loadStyle(const boost::filesystem::path& path);

  std::unique_ptr<Sosofo> processNode(const Node* node);
};

} // ns eyestep
