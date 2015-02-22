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


TEST_CASE("Unknown properties report as default", "[nodes]")
{
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
  REQUIRE(nd.property<eyestep::NodeList>("kids")[0]->gi() == "foo");
  REQUIRE(nd.property<eyestep::NodeList>("kids")[1]->gi() == "bar");
}


TEST_CASE("Traverse", "[nodes]")
{
  eyestep::Node nd("foo");
  nd["name"] = "bar";
  nd["size"] = 42;

  eyestep::Node type("type");
  type["const?"] = true;

  eyestep::Node args("args");
  args.addNode("params", eyestep::Node("p1"));
  args.addNode("params", eyestep::Node("p2"));
  args.addNode("params", eyestep::Node("p3"));

  nd.addNode("kids", eyestep::Node("title"));
  nd.addNode("kids", args);
  nd.addNode("types", type);

  std::vector<std::string> gis;
  eyestep::nodeTraverse(nd, [&gis](const eyestep::Node& nd) {
    gis.emplace_back(nd.gi());
  });

  REQUIRE(gis == (std::vector<std::string>{"foo", "title", "args", "p1", "p2",
                                           "p3", "type"}));
}
