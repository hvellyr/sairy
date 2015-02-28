// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "catch/catch.hpp"

#include "../nodes.hpp"
#include "../nodelist.hpp"

#include <string>


TEST_CASE("Empty nodelist", "[nodelist]")
{
  eyestep::NodeList nl;

  REQUIRE(nl.length() == 0);
  REQUIRE(nl.rest().length() == 0);
}


TEST_CASE("NodeList of all children", "[nodelist]")
{
  eyestep::Grove grove;
  auto* nd = grove.makeNode("foo");

  nd->setProperty("name", "bar");
  nd->setProperty("size", 42);

  auto* title = grove.makeNode("title");

  auto* args = grove.makeNode("args");
  args->addNode("params", grove.makeNode("p1"));
  args->addNode("params", grove.makeNode("p2"));
  args->addNode("params", grove.makeNode("p3"));

  auto* gaz = grove.makeNode("gaz");
  args->addChildNode(gaz);
  auto* moo = grove.makeNode("moo");
  gaz->addChildNode(moo);

  auto* type = grove.makeNode("type");
  type->setProperty("const?", true);

  nd->addChildNode(title);
  nd->addChildNode(args);
  nd->addChildNode(type);


  SECTION("Children")
  {
    auto nl = eyestep::NodeList(nd, eyestep::NodeList::kChildren);
    REQUIRE(nl.length() == 3);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
    REQUIRE(nl.rest().rest().head() == type);
  }

  SECTION("Siblings middle")
  {
    auto nl = eyestep::NodeList(args, eyestep::NodeList::kSiblings);
    REQUIRE(nl.length() == 3);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
    REQUIRE(nl.rest().rest().head() == type);
  }

  SECTION("Siblings @ start")
  {
    auto nl = eyestep::NodeList(title, eyestep::NodeList::kSiblings);
    REQUIRE(nl.length() == 3);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
    REQUIRE(nl.rest().rest().head() == type);
  }

  SECTION("Siblings @ end")
  {
    auto nl = eyestep::NodeList(type, eyestep::NodeList::kSiblings);
    REQUIRE(nl.length() == 3);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
    REQUIRE(nl.rest().rest().head() == type);
  }

  SECTION("Children on a leaf")
  {
    auto nl = eyestep::NodeList(moo, eyestep::NodeList::kChildren);
    REQUIRE(nl.length() == 0);
  }


  SECTION("Preced @ start")
  {
    auto nl = eyestep::NodeList(title, eyestep::NodeList::kPreced);
    REQUIRE(nl.length() == 0);
  }

  SECTION("Preced @ middle")
  {
    auto nl = eyestep::NodeList(args, eyestep::NodeList::kPreced);
    REQUIRE(nl.length() == 1);
    REQUIRE(nl.head() == title);
  }
  SECTION("Preced @ end")
  {
    auto nl = eyestep::NodeList(type, eyestep::NodeList::kPreced);
    REQUIRE(nl.length() == 2);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
  }


  SECTION("Follow @ start")
  {
    auto nl = eyestep::NodeList(title, eyestep::NodeList::kFollow);
    REQUIRE(nl.length() == 2);
    REQUIRE(nl.head() == args);
    REQUIRE(nl.rest().head() == type);
  }

  SECTION("Follow @ middle")
  {
    auto nl = eyestep::NodeList(args, eyestep::NodeList::kFollow);
    REQUIRE(nl.length() == 1);
    REQUIRE(nl.head() == type);
  }
  SECTION("Follow @ end")
  {
    auto nl = eyestep::NodeList(type, eyestep::NodeList::kFollow);
    REQUIRE(nl.length() == 0);
  }


  SECTION("Ancestors")
  {
    auto nl = eyestep::NodeList(gaz, eyestep::NodeList::kAncestors);
    REQUIRE(nl.length() == 2);
    // calling length() again, gives the same result
    REQUIRE(nl.length() == 2);
    REQUIRE(nl.head() == args);
    REQUIRE(nl.rest().length() == 1);
    REQUIRE(nl.rest().head() == nd);
    REQUIRE(nl.rest().rest().length() == 0);
  }

  SECTION("Ancestors from root")
  {
    auto nl = eyestep::NodeList(nd, eyestep::NodeList::kAncestors);
    REQUIRE(nl.length() == 0);
  }


  SECTION("Descendants")
  {
    auto nl = eyestep::NodeList(nd, eyestep::NodeList::kDescendants);

    REQUIRE(nl.length() == 5);
    REQUIRE(nl.head() == title);

    auto n2 = nl.rest();
    REQUIRE(n2.length() == 4);
    REQUIRE(n2.head() == args);

    auto n3 = nl.rest().rest();
    REQUIRE(n3.length() == 3);
    REQUIRE(n3.head() == gaz);

    auto n4 = nl.rest().rest().rest();
    REQUIRE(n4.length() == 2);
    REQUIRE(n4.head() == moo);

    auto n5 = nl.rest().rest().rest().rest();
    REQUIRE(n5.length() == 1);
    REQUIRE(n5.head() == type);

    REQUIRE(nl.length() == 5);
  }


  SECTION("Descendants from leaf")
  {
    auto nl = eyestep::NodeList(moo, eyestep::NodeList::kDescendants);
    REQUIRE(nl.length() == 0);
  }
}
