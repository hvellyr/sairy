// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "program_options/program_options.hpp"

#include <memory>

namespace eyestep {

class IProcessor;

program_options::options_description processor_options();

std::unique_ptr<eyestep::IProcessor>
make_processor_for_file(const std::string& proc_id,
                        const program_options::variables_map& args);

} // ns eyestep
