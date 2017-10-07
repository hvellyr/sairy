// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "program_options/program_options.hpp"

#include "fspp/filesystem.hpp"

#include <string>
#include <unordered_set>
#include <vector>


namespace eyestep {

class Node;
class Grove;

class IScanner
{
public:
  virtual ~IScanner() {}

  virtual std::string scanner_id() const = 0;
  virtual std::unordered_set<std::string> supported_extensions() const = 0;

  virtual program_options::options_description program_options() const = 0;

  virtual Node* scan_file(eyestep::Grove& grove, const filesystem::path& srcfile) = 0;
};

} // ns eyestep
