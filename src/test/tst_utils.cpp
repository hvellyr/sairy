// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "catch/catch.hpp"

#include "../utils.hpp"

#include "fspp/filesystem.hpp"

#include <iostream>

namespace fs = eyestep::filesystem;


TEST_CASE("Make relative paths", "[utils]")
{
  REQUIRE(fs::path("assets/pic.png") ==
          eyestep::utils::make_relative(fs::path("data/"),
                                        fs::path("data/assets/pic.png")));
}

TEST_CASE("Relative paths with volumes", "[utils]")
{
  REQUIRE(fs::path("assets/pic.png") ==
          eyestep::utils::make_relative(fs::path("c:/data/"),
                                        fs::path("c:/data/assets/pic.png")));
}
