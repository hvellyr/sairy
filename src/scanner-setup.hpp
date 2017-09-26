// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "program_options/program_options.hpp"

#include <boost/filesystem.hpp>

#include <memory>

namespace eyestep {

class IScanner;

program_options::options_description scanner_options();

std::unique_ptr<eyestep::IScanner>
make_scanner_for_file(const boost::filesystem::path& file,
                      const program_options::variables_map& args);

} // ns eyestep
