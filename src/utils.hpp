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

  boost::filesystem::path make_relative(const boost::filesystem::path& from,
                                        const boost::filesystem::path& to);

  std::u16string utf8_to_u16string(const std::string& str);
  std::string u16string_to_utf8(const std::u16string& str);
} // ns utils
} // ns eyestep
