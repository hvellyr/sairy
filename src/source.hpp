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
         const std::vector<std::string>& locIncls,
         const std::vector<std::string>& locDefines)
      : mSrcfile(srcfile), mLocIncls(locIncls), mLocDefs(locDefines)
  {
  }

  boost::filesystem::path mSrcfile;
  std::vector<std::string> mLocIncls;
  std::vector<std::string> mLocDefs;
};

std::vector<Source> readScanDb(const fs::path& file);

} // ns eyestep
