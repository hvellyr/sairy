// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/program_options.hpp>

#include <memory>

namespace eyestep {

class IProcessor;

boost::program_options::options_description processor_options();

std::unique_ptr<eyestep::IProcessor>
make_processor_for_file(const std::string& proc_id,
                        const boost::program_options::variables_map& args);

} // ns eyestep
