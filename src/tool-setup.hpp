// Copyright (c) 2020 Gregor Klinke
// All rights reserved.

#pragma once

#include "fspp/filesystem.hpp"

#include <vector>

namespace eyestep {

class ToolSetup
{
public:
  std::vector<filesystem::path> _prefix_path;
};

} // namespace eyestep
