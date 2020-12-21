// Copyright (c) 2019 Gregor Klinke
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


class LexiconScanner : public IScanner
{
  bool _debug = false;

public:
  LexiconScanner();
  LexiconScanner(const ToolSetup& setup, const cxxopts::ParseResult& /*args*/);

  std::string scanner_id() const override {
    return "lexicon";
  }

  std::unordered_set<std::string> supported_extensions() const override {
    return {".lexicon", ".tlx"};
  }

  void add_program_options(cxxopts::Options& options) const override;

  Node* scan_file(eyestep::Grove& grove, const filesystem::path& srcfile) override;
};

} // ns eyestep
