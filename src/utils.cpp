// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "utils.hpp"

#include <boost/range/iterator_range.hpp>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

namespace eyestep {
namespace utils {

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


  std::vector<std::string> joinList(const std::vector<std::string>& one,
                                    const std::vector<std::string>& sec)
  {
    std::vector<std::string> result;
    result.insert(result.end(), one.begin(), one.end());
    result.insert(result.end(), sec.begin(), sec.end());
    return result;
  }

} // ns utils
} // ns eyestep
