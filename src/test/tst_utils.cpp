// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "catch/catch.hpp"

#include "../utils.hpp"

#include "fspp/filesystem.hpp"

#include <string>


namespace eyestep {
namespace utils {

namespace fs = filesystem;


TEST_CASE("split_path", "[string][utils]")
{
  REQUIRE((std::vector<fs::path>{"abc/def", "/foo/bar"}) ==
          split_paths("abc/def:/foo/bar"));
  REQUIRE((std::vector<fs::path>{}) == split_paths(""));
  REQUIRE((std::vector<fs::path>{"abc/def", "/foo/bar", "~/"}) ==
          split_paths("abc/def:/foo/bar:~/"));
}


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
  using namespace std;

  REQUIRE(string("hello world") == trim_copy("   hello world "));
  REQUIRE(string("hello world") == trim_copy("   hello world"));
  REQUIRE(string("hello world") == trim_copy("   hello world   "));
  REQUIRE(string("hello world") == trim_copy("hello world   "));
  REQUIRE(string("hello   world") == trim_copy("hello   world\0x09   "));
  REQUIRE(string("hello   world") == trim_copy("\n\rhello   world\0x09   \n"));
}


TEST_CASE("trim_left", "[string][utils]")
{
  using namespace std;

  REQUIRE(string("hello world ") == trim_left_copy("   hello world "));
  REQUIRE(string("hello world") == trim_left_copy("   hello world"));
  REQUIRE(string("hello world   ") == trim_left_copy("   hello world   "));
  REQUIRE(string("hello world   ") == trim_left_copy("hello world   "));
  REQUIRE(string("hello   world\0x09   ") == trim_left_copy("hello   world\0x09   "));
  REQUIRE(string("hello   world\0x09   \n") == trim_left_copy("\n\rhello   world\0x09   \n"));
}


TEST_CASE("trim_right", "[string][utils]")
{
  using namespace std;

  REQUIRE(string("   hello world") == trim_right_copy("   hello world "));
  REQUIRE(string("   hello world") == trim_right_copy("   hello world"));
  REQUIRE(string("   hello world") == trim_right_copy("   hello world   "));
  REQUIRE(string("hello world") == trim_right_copy("hello world   "));
  REQUIRE(string("\0x09hello   world") == trim_right_copy("\0x09hello   world\0x09   "));
  REQUIRE(string("\n\rhello   world") == trim_right_copy("\n\rhello   world\0x09   \n"));
}


TEST_CASE("join", "[string][utils]")
{
  using namespace std;

  REQUIRE(string("abc:def:x") == join(vector<string>{"abc", "def", "x"}, ":"));
  REQUIRE(string("a") == join(vector<string>{"a"}, ":"));
  REQUIRE(string("") == join(vector<string>{}, ":"));
  REQUIRE(string("") == join(vector<string>{}, ""));
  REQUIRE(string("|42") == join(vector<string>{"", "42"}, "|"));
}


TEST_CASE("split", "[string][utils]")
{
  using namespace std;

  REQUIRE((vector<string>{"hello", "world"}) == split("hello world", " "));
  REQUIRE((vector<string>{"abc", "def", "x"}) == split("  abc   def       x  ", " "));
  REQUIRE((vector<string>{"abc"}) == split("  abc   ", " "));
  REQUIRE((vector<string>{"abc", "def", "x"}) == split("<abc><def><x>", "<>"));
  REQUIRE((vector<string>{}) == split("", " "));

  REQUIRE((vector<string>{"abc", "def", "x"}) == split("  abc ;  def ;      x  ", ";", true));
  REQUIRE((vector<string>{"abc", "def", "", "x"}) == split("  abc ;  def ; ;   x  ", ";", true));
}

} // namespace utils
} // namespace eyestep
