// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "catch/catch.hpp"

#include "../utils.hpp"

#include "fspp/filesystem.hpp"

#include <string>


namespace eyestep {
namespace utils {

namespace fs = filesystem;


TEST_CASE("Make relative paths", "[utils]")
{
  REQUIRE(fs::path("assets/pic.png") ==
          make_relative(fs::path("data/"),
                        fs::path("data/assets/pic.png")));
}


TEST_CASE("Relative paths with volumes", "[utils]")
{
  REQUIRE(fs::path("assets/pic.png") ==
          make_relative(fs::path("c:/data/"),
                        fs::path("c:/data/assets/pic.png")));
}


TEST_CASE("trim", "[string][utils]")
{
  REQUIRE(std::string("hello world") == trim_copy("   hello world "));
  REQUIRE(std::string("hello world") == trim_copy("   hello world"));
  REQUIRE(std::string("hello world") == trim_copy("   hello world   "));
  REQUIRE(std::string("hello world") == trim_copy("hello world   "));
  REQUIRE(std::string("hello   world") == trim_copy("hello   world\0x09   "));
  REQUIRE(std::string("hello   world") == trim_copy("\n\rhello   world\0x09   \n"));
}


TEST_CASE("trim_left", "[string][utils]")
{
  REQUIRE(std::string("hello world ") == trim_left_copy("   hello world "));
  REQUIRE(std::string("hello world") == trim_left_copy("   hello world"));
  REQUIRE(std::string("hello world   ") == trim_left_copy("   hello world   "));
  REQUIRE(std::string("hello world   ") == trim_left_copy("hello world   "));
  REQUIRE(std::string("hello   world\0x09   ") == trim_left_copy("hello   world\0x09   "));
  REQUIRE(std::string("hello   world\0x09   \n") == trim_left_copy("\n\rhello   world\0x09   \n"));
}


TEST_CASE("trim_right", "[string][utils]")
{
  REQUIRE(std::string("   hello world") == trim_right_copy("   hello world "));
  REQUIRE(std::string("   hello world") == trim_right_copy("   hello world"));
  REQUIRE(std::string("   hello world") == trim_right_copy("   hello world   "));
  REQUIRE(std::string("hello world") == trim_right_copy("hello world   "));
  REQUIRE(std::string("\0x09hello   world") == trim_right_copy("\0x09hello   world\0x09   "));
  REQUIRE(std::string("\n\rhello   world") == trim_right_copy("\n\rhello   world\0x09   \n"));
}

} // namespace utils
} // namespace eyestep
