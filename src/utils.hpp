// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fspp/filesystem.hpp"

#include <string>
#include <vector>


namespace eyestep {
namespace utils {

  std::string join(const std::vector<std::string>& strlist,
                   const std::string& gap);

  std::vector<std::string> join_list(const std::vector<std::string>& one,
                                     const std::vector<std::string>& sec);

  std::vector<filesystem::path> split_paths(const std::string& path);

  filesystem::path make_relative(const filesystem::path& from,
                                 const filesystem::path& to);

  std::u16string utf8_to_u16string(const std::string& str);
  std::string u16string_to_utf8(const std::u16string& str);

  std::u32string utf8_to_u32string(const std::string& str);
  std::string u32string_to_utf8(const std::u32string& str);

  std::string to_lower(const std::string& src);
  std::string to_upper(const std::string& src);

} // ns utils
} // ns eyestep
