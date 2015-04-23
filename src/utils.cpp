// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "utils.hpp"

#include <boost/algorithm/string/classification.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/filesystem.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/iterator.hpp>
#include <boost/range/iterator_range.hpp>

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

namespace eyestep {
namespace utils {

  namespace fs = boost::filesystem;

  std::string join(const std::vector<std::string>& strlist,
                   const std::string& gap)
  {
    std::stringstream result;

    if (!strlist.empty()) {
      result << strlist.front();

      for (const auto& str :
           boost::make_iterator_range(boost::next(strlist.begin()),
                                      strlist.end())) {
        result << gap << str;
      }
    }

    return result.str();
  }


  std::vector<std::string> join_list(const std::vector<std::string>& one,
                                     const std::vector<std::string>& sec)
  {
    std::vector<std::string> result;
    result.insert(result.end(), one.begin(), one.end());
    result.insert(result.end(), sec.begin(), sec.end());
    return result;
  }


  std::vector<fs::path> split_paths(const std::string& path)
  {
    using namespace boost::adaptors;
    using namespace boost::algorithm;

    std::vector<std::string> steps;
    split(steps, path, is_any_of(":"), token_compress_on);

    return boost::copy_range<std::vector<fs::path>>(
      steps |
      transformed([](const std::string& value) { return fs::path(value); }));
  }


  // Return path when appended to a_From will resolve to same as a_To
  fs::path make_relative(const fs::path& from, const fs::path& to)
  {
    auto _from = fs::absolute(from);
    auto _to = fs::absolute(to);

    fs::path ret;
    fs::path::const_iterator i_from(_from.begin());
    fs::path::const_iterator i_to(_to.begin());

    // Find common base
    for (fs::path::const_iterator to_end(_to.end()), from_end(_from.end());
         i_from != from_end && i_to != to_end && *i_from == *i_to;
         ++i_from, ++i_to) {
    }
    // Navigate backwards in directory to reach previously found base
    for (fs::path::const_iterator from_end(_from.end()); i_from != from_end;
         ++i_from) {
      if ((*i_from) != ".") {
        ret /= "..";
      }
    }
    // Now navigate down the directory branch
    for ( ; i_to != _to.end() ; ++i_to) {
      ret /= *i_to;
    }
    //ret.append(i_to, _to.end());
    return ret;
  }

} // ns utils
} // ns eyestep
