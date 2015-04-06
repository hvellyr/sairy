// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "utils.hpp"

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/optional.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/regex.hpp>

#include <iostream>
#include <string>


namespace eyestep {

namespace {
  std::vector<std::string> split_lines(const std::string& text)
  {
    std::vector<std::string> lines;
    boost::algorithm::split(lines, text, boost::algorithm::is_any_of("\n\r"),
                            boost::algorithm::token_compress_on);

    return lines;
  }

  boost::optional<int> get_indent(const std::string& line,
                                  const boost::regex& prefix)
  {
    boost::smatch what;
    if (boost::regex_match(line, what, prefix)) {
      if (what[1].length() == line.size()) {
        return boost::none;
      }
      return what[2].length();
    }

    return 0;
  }

  int get_common_indent(const std::vector<std::string>& lines,
                        const boost::regex& first_prefix,
                        const boost::regex& nth_prefix)
  {
    int common_indent = 80;
    if (!lines.empty()) {
      auto indent = get_indent(lines[0], first_prefix);
      if (indent) {
        common_indent = std::min<int>(common_indent, *indent);
      }

      for (const auto& ln :
           boost::make_iterator_range(lines.begin() + 1, lines.end())) {
        auto indent = get_indent(ln, nth_prefix);

        if (indent) {
          common_indent = std::min<int>(common_indent, *indent);
        }
      }

      return common_indent;
    }

    return 0;
  }


  //----------------------------------------------------------------------------

  std::vector<std::string>
  strip_last_line(const std::vector<std::string>& lines,
                  const boost::regex& last_prefix,
                  const boost::regex& last_suffix)
  {
    std::vector<std::string> result;

    if (!lines.empty()) {
      if (lines.size() > 1) {
        result.insert(result.begin(), lines.begin(), lines.end() - 1);
      }

      if (!boost::regex_match(lines.back(), last_prefix)) {
        boost::smatch what;
        if (boost::regex_match(lines.back(), what, last_suffix)) {
          result.emplace_back(what[1].str());
        }
        else {
          result.emplace_back(lines.back());
        }
      }
    }

    return result;
  }

  //----------------------------------------------------------------------------

  std::string normalize_line(const std::string& line, int prefix_length,
                             int common_indent)
  {
    int strip_indent = prefix_length + common_indent;
    if (line.size() > strip_indent) {
      return line.substr(strip_indent, line.size());
    }
    else if (line.size() == strip_indent) {
      return std::string();
    }
    else if (line.size() >= prefix_length) {
      return line.substr(prefix_length, line.size());
    }
    else {
      return line;
    }
  }

  std::vector<std::string> strip_lines(const std::vector<std::string>& lines,
                                       const boost::regex& first_prefix,
                                       const boost::regex& nth_prefix,
                                       int common_indent)
  {
    std::vector<std::string> stripped_lines;
    if (!lines.empty()) {
      boost::smatch what;
      if (boost::regex_match(lines[0], what, first_prefix)) {
        stripped_lines.emplace_back(
          normalize_line(lines[0], what[1].length(), common_indent));
      }

      for (const auto& ln :
           boost::make_iterator_range(lines.begin() + 1, lines.end())) {
        boost::smatch what;
        if (boost::regex_match(ln, what, nth_prefix)) {
          stripped_lines.emplace_back(
            normalize_line(ln, what[1].length(), common_indent));
        }
      }
    }
    return stripped_lines;
  }

  //----------------------------------------------------------------------------

  std::string normalize_c_style(const std::string& orig,
                                const boost::regex& first_prefix,
                                const boost::regex& nth_prefix,
                                const boost::regex& last_prefix,
                                const boost::regex& last_suffix)
  {
    auto lines = strip_last_line(split_lines(orig), last_prefix, last_suffix);
    auto common_indent = get_common_indent(lines, first_prefix, nth_prefix);

    return utils::join(strip_lines(lines, first_prefix, nth_prefix,
                                   common_indent),
                       "\n");
  }

  std::string normalize_cpp_style(const std::string& orig,
                                  const boost::regex& prefix)
  {
    auto lines = split_lines(orig);
    auto common_indent = get_common_indent(lines, prefix, prefix);

    return utils::join(strip_lines(lines, prefix, prefix, common_indent), "\n");
  }
} // anon ns


std::string normalize_comment(const std::string& orig)
{
  auto com = boost::trim_copy(orig);

  if (boost::starts_with(com, "/**")) {
    return normalize_c_style(com, boost::regex("^(/\\*\\*)(\\s*).*"),
                             boost::regex("^(\\s*\\*)(\\s*).*"),
                             boost::regex("^(\\s*\\*/)"),
                             boost::regex("^(.*)\\*/"));
  }
  else if (boost::starts_with(com, "/*!")) {
    return normalize_c_style(com, boost::regex("^(/\\*!)(\\s*).*"),
                             boost::regex("^(\\s*\\*)(\\s*).*"),
                             boost::regex("^(\\s*\\*/)"),
                             boost::regex("^(.*)\\*/"));
  }
  else if (boost::starts_with(com, "///")) {
    return normalize_cpp_style(com, boost::regex("^(///)(\\s*).*"));
  }
  else if (boost::starts_with(com, "//!")) {
    return normalize_cpp_style(com, boost::regex("^(//!)(\\s*).*"));
  }

  return std::string();
}

} // ns eyestep
