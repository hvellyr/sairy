// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <string>


namespace eyestep {

namespace fo {
  struct Color;
  
  Color color_by_x11name(const std::string& color_name);
} // namespace fo

} // ns eyestep
