// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <string>
#include <vector>

namespace eyestep {

class Source {
public:
  Source(const std::string& srcfile, const std::vector<std::string>& locIncls,
         const std::vector<std::string>& locDefines)
      : mSrcfile(srcfile), mLocIncls(locIncls), mLocDefs(locDefines)
  {
  }

  std::string mSrcfile;
  std::vector<std::string> mLocIncls;
  std::vector<std::string> mLocDefs;
};

std::vector<Source> readScanDb(const std::string& file);

} // ns eyestep
