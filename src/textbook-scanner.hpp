// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "scanner.hpp"

#include "cxxopts.hpp"
#include "fspp/filesystem.hpp"

#include <string>
#include <unordered_set>
#include <vector>


namespace eyestep {

class Node;
class Grove;
class ToolSetup;


class TextbookScanner : public IScanner
{
  bool _debug = false;
  std::vector<filesystem::path> _prefix_path;
  std::vector<filesystem::path> _catalog_path;

public:
  TextbookScanner();
  TextbookScanner(const ToolSetup& setup, const cxxopts::ParseResult& /*args*/);

  std::string scanner_id() const override {
    return "textbook";
  }

  std::unordered_set<std::string> supported_extensions() const override {
    return {".tb", ".textbook"};
  }

  void add_program_options(cxxopts::Options& options) const override;

  Node* scan_file(eyestep::Grove& grove, const filesystem::path& srcfile) override;
};

} // namespace eyestep
