// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "scanner.hpp"

#include <boost/program_options.hpp>

#include <string>
#include <vector>
#include <unordered_set>

namespace eyestep {

class Node;
class Grove;

class MarkdownScanner : public IScanner {
public:
  MarkdownScanner();
  MarkdownScanner(const boost::program_options::variables_map& args);

  std::string scanner_id() const override;
  std::unordered_set<std::string> supported_extensions() const override;
  boost::program_options::options_description program_options() const override;

  Node* scan_file(eyestep::Grove& grove,
                  const boost::filesystem::path& srcfile) override;
};

} // ns eyestep
