// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fspp/filesystem.hpp"

#include <string>
#include <vector>


namespace eyestep {

class Source {
public:
  Source(const filesystem::path& srcfile,
         const std::vector<std::string>& locincls,
         const std::vector<std::string>& locdefs)
    : _srcfile(srcfile), _locincls(locincls), _locdefs(locdefs)
  {
  }

  filesystem::path _srcfile;
  std::vector<std::string> _locincls;
  std::vector<std::string> _locdefs;
};

std::vector<Source> read_scan_db(const filesystem::path& file);

} // ns eyestep
