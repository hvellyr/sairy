// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fspp/filesystem.hpp"

#include <string>
#include <vector>


namespace eyestep {
namespace utils {

  std::string join(const std::vector<std::string>& strlist, const std::string& gap);

  std::vector<std::string> split(const std::string& str, const std::string& seps,
                                 bool trim_token = false);
  std::vector<std::string> split_str(const std::string& str, const std::string& seps,
                                     bool trim_token = false);

  std::vector<std::string> join_list(const std::vector<std::string>& one,
                                     const std::vector<std::string>& sec);

  std::vector<filesystem::path> split_paths(const std::string& path);

  std::string replace_str(const std::string& src, const std::string& pattern,
                          const std::string& replcm);

  filesystem::path make_relative(const filesystem::path& from,
                                 const filesystem::path& to);

  std::u32string utf8_to_u32string(const std::string& str);
  std::string u32string_to_utf8(const std::u32string& str);

  std::string to_lower(const std::string& src);
  std::string to_upper(const std::string& src);

  std::u32string& trim_left(std::u32string& src);
  std::u32string& trim_right(std::u32string& src);
  std::u32string& trim(std::u32string& src);

  std::string trim_left_copy(const std::string& src);
  std::string trim_right_copy(const std::string& src);
  std::string trim_copy(const std::string& src);

} // ns utils
} // ns eyestep
