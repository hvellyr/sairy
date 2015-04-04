// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include <memory>

namespace eyestep {

class IScanner;

std::vector<boost::program_options::options_description> scanner_options();

std::unique_ptr<eyestep::IScanner>
make_scanner_for_file(const boost::filesystem::path& file,
                      const boost::program_options::variables_map& args);

} // ns eyestep
