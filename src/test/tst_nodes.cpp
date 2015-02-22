// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "catch/catch.hpp"

#include "../nodes.hpp"

#include <string>


TEST_CASE("Base node creation", "[nodes]")
{
  eyestep::Node nd("blue");
  REQUIRE(nd.gi() == "blue");
}


TEST_CASE("Default node as no gi", "[nodes]")
{
  eyestep::Node nd;
  REQUIRE(nd.gi().empty());
}


TEST_CASE("Set properties", "[nodes]")
{
  eyestep::Node nd;
  nd["name"] = "foo";
  nd["size"] = 42;

  REQUIRE(nd.property<std::string>("name") == "foo");
  REQUIRE(nd.property<int>("size") == 42);
}


TEST_CASE("Unknown properties report as default", "[nodes]") {
  eyestep::Node nd;

  REQUIRE(nd.property<std::string>("name").empty());
  REQUIRE(!nd.hasProperty("name"));
}


TEST_CASE("Add node", "[nodes]")
{
  eyestep::Node nd;

  nd.addNode("kids", eyestep::Node("foo"));
  nd.addNode("kids", eyestep::Node("bar"));

  REQUIRE(nd.property<eyestep::NodeList>("kids").size() == 2u);
  REQUIRE(nd.property<eyestep::NodeList>("kids")[0].gi() == "foo");
  REQUIRE(nd.property<eyestep::NodeList>("kids")[1].gi() == "bar");
}
