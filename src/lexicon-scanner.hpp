// Copyright (c) 2019 Gregor Klinke
// All rights reserved.

#pragma once

#include "scanner.hpp"

#include "program_options/program_options.hpp"

#include "fspp/filesystem.hpp"

#include <string>
#include <unordered_set>
#include <vector>


namespace eyestep {

class Node;
class Grove;

class LexiconScanner : public IScanner
{
  bool _debug = false;

public:
  LexiconScanner();
  LexiconScanner(const program_options::variables_map& /*args*/);

  std::string scanner_id() const override {
    return "lexicon";
  }

  std::unordered_set<std::string> supported_extensions() const override {
    return {".lexicon", ".tlx"};
  }

  program_options::options_description program_options() const override;

  Node* scan_file(eyestep::Grove& grove, const filesystem::path& srcfile) override;
};

} // ns eyestep
