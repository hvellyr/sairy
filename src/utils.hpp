// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <vector>
#include <string>


namespace eyestep {
namespace utils {

  std::string join(const std::vector<std::string>& strlist,
                   const std::string& gap);

  std::vector<std::string> joinList(const std::vector<std::string>& one,
                                    const std::vector<std::string>& sec);

} // ns utils
} // ns eyestep
