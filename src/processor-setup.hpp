// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "cxxopts.hpp"

#include <memory>
#include <string>
#include <vector>


namespace eyestep {

class IProcessor;

std::vector<std::string> all_processors();
void add_processor_options(cxxopts::Options& options);

std::unique_ptr<eyestep::IProcessor>
make_processor(const std::string& proc_id, const cxxopts::ParseResult& args);

} // ns eyestep
