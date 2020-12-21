// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "cxxopts.hpp"
#include "fspp/filesystem.hpp"

#include <memory>
#include <string>
#include <vector>


namespace eyestep {

class IScanner;
class ToolSetup;


std::vector<std::string> all_scanners();
void add_scanner_options(cxxopts::Options& options);

std::unique_ptr<eyestep::IScanner>
make_scanner_for_file(const filesystem::path& file, const ToolSetup& setup,
                      const cxxopts::ParseResult& args);

} // namespace eyestep
