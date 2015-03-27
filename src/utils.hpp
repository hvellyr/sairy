// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/filesystem.hpp>
#include <string>
#include <vector>


namespace eyestep {
namespace utils {

  std::string join(const std::vector<std::string>& strlist,
                   const std::string& gap);

  std::vector<std::string> join_list(const std::vector<std::string>& one,
                                     const std::vector<std::string>& sec);

  std::vector<boost::filesystem::path> split_paths(const std::string& path);

} // ns utils
} // ns eyestep
