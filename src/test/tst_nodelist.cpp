// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "catch/catch.hpp"

#include "../nodeclass.hpp"
#include "../nodelist.hpp"
#include "../nodes.hpp"

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
  auto* nd = grove.make_elt_node("foo");

  nd->set_property("name", "bar");
  nd->set_property("size", 42);

  auto* title = grove.make_elt_node("title");

  auto* args = grove.make_elt_node("args");
  args->add_node("params", grove.make_elt_node("p1"));
  args->add_node("params", grove.make_elt_node("p2"));
  args->add_node("params", grove.make_elt_node("p3"));

  auto* gaz = grove.make_elt_node("gaz");
  args->add_child_node(gaz);
  auto* moo = grove.make_elt_node("moo");
  gaz->add_child_node(moo);

  auto* type = grove.make_elt_node("type");
  type->set_property("const?", true);

  nd->add_child_node(title);
  nd->add_child_node(args);
  nd->add_child_node(type);


  SECTION("Children")
  {
    auto nl = eyestep::NodeList(nd, eyestep::NodeList::k_children);
    REQUIRE(nl.length() == 3);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
    REQUIRE(nl.rest().rest().head() == type);
  }

  SECTION("Siblings middle")
  {
    auto nl = eyestep::NodeList(args, eyestep::NodeList::k_siblings);
    REQUIRE(nl.length() == 3);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
    REQUIRE(nl.rest().rest().head() == type);
  }

  SECTION("Siblings @ start")
  {
    auto nl = eyestep::NodeList(title, eyestep::NodeList::k_siblings);
    REQUIRE(nl.length() == 3);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
    REQUIRE(nl.rest().rest().head() == type);
  }

  SECTION("Siblings @ end")
  {
    auto nl = eyestep::NodeList(type, eyestep::NodeList::k_siblings);
    REQUIRE(nl.length() == 3);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
    REQUIRE(nl.rest().rest().head() == type);
  }

  SECTION("Children on a leaf")
  {
    auto nl = eyestep::NodeList(moo, eyestep::NodeList::k_children);
    REQUIRE(nl.length() == 0);
  }


  SECTION("Preced @ start")
  {
    auto nl = eyestep::NodeList(title, eyestep::NodeList::k_preced);
    REQUIRE(nl.length() == 0);
  }

  SECTION("Preced @ middle")
  {
    auto nl = eyestep::NodeList(args, eyestep::NodeList::k_preced);
    REQUIRE(nl.length() == 1);
    REQUIRE(nl.head() == title);
  }
  SECTION("Preced @ end")
  {
    auto nl = eyestep::NodeList(type, eyestep::NodeList::k_preced);
    REQUIRE(nl.length() == 2);
    REQUIRE(nl.head() == title);
    REQUIRE(nl.rest().head() == args);
  }


  SECTION("Follow @ start")
  {
    auto nl = eyestep::NodeList(title, eyestep::NodeList::k_follow);
    REQUIRE(nl.length() == 2);
    REQUIRE(nl.head() == args);
    REQUIRE(nl.rest().head() == type);
  }

  SECTION("Follow @ middle")
  {
    auto nl = eyestep::NodeList(args, eyestep::NodeList::k_follow);
    REQUIRE(nl.length() == 1);
    REQUIRE(nl.head() == type);
  }
  SECTION("Follow @ end")
  {
    auto nl = eyestep::NodeList(type, eyestep::NodeList::k_follow);
    REQUIRE(nl.length() == 0);
  }


  SECTION("Ancestors")
  {
    auto nl = eyestep::NodeList(gaz, eyestep::NodeList::k_ancestors);
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
    auto nl = eyestep::NodeList(nd, eyestep::NodeList::k_ancestors);
    REQUIRE(nl.length() == 0);
  }


  SECTION("Descendants")
  {
    auto nl = eyestep::NodeList(nd, eyestep::NodeList::k_descendants);

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
    auto nl = eyestep::NodeList(moo, eyestep::NodeList::k_descendants);
    REQUIRE(nl.length() == 0);
  }
}
