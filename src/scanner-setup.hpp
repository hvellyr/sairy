// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/filesystem.hpp>

#include <memory>

namespace eyestep {

class IScanner;

std::unique_ptr<eyestep::IScanner>
make_scanner_for_file(const boost::filesystem::path& file);

} // ns eyestep
