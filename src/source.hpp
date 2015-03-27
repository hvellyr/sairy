// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/filesystem.hpp>

#include <string>
#include <vector>


namespace eyestep {

namespace fs = boost::filesystem;

class Source {
public:
  Source(const boost::filesystem::path& srcfile,
         const std::vector<std::string>& locincls,
         const std::vector<std::string>& locdefs)
    : _srcfile(srcfile), _locincls(locincls), _locdefs(locdefs)
  {
  }

  boost::filesystem::path _srcfile;
  std::vector<std::string> _locincls;
  std::vector<std::string> _locdefs;
};

std::vector<Source> read_scan_db(const fs::path& file);

} // ns eyestep
