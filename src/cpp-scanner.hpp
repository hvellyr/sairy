// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "scanner.hpp"
#include <boost/program_options.hpp>

#include <string>
#include <vector>
#include <unordered_set>

namespace eyestep {

namespace fs = boost::filesystem;

class Node;
class Grove;

class CppScanner : public IScanner {
  std::vector<std::string> _incl_paths;
  std::vector<std::string> _defs;
  bool _verbose;

public:
  CppScanner();
  CppScanner(const boost::program_options::variables_map& args);

  std::string scanner_id() const override;
  std::unordered_set<std::string> supported_extensions() const override;
  boost::program_options::options_description program_options() const override;

  Node* scan_file(eyestep::Grove& grove, const fs::path& srfile) override;
};

} // ns eyestep
