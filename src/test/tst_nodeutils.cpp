// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "catch/catch.hpp"

#include "../nodeclass.hpp"
#include "../nodes.hpp"
#include "../nodeutils.hpp"

#include <string>
#include <sstream>
#include <tuple>


using namespace eyestep;


TEST_CASE("Traverse", "[nodes][traverse]")
{
  Grove grove;
  auto* nd = grove.make_elt_node("foo");

  nd->set_property("name", "bar");
  nd->set_property("size", 42);

  auto* type = grove.make_elt_node("type");
  type->set_property("const?", true);

  auto* args = grove.make_elt_node("args");
  args->add_node("params", grove.make_elt_node("p1"));
  args->add_node("params", grove.make_elt_node("p2"));
  args->add_node("params", grove.make_elt_node("p3"));

  nd->add_child_node(grove.make_elt_node("title"));
  nd->add_child_node(args);
  nd->add_child_node(type);

  SECTION("Full recursion")
  {
    using GiType = std::tuple<std::string, int>;
    using ExpectedGiType = std::vector<GiType>;
    ExpectedGiType gis;

    node_traverse(nd, [&gis](const Node* n, int depth) {
      gis.emplace_back(std::make_tuple(n->gi(), depth));
      return TraverseRecursion::k_recurse;
    });

    REQUIRE(gis == (ExpectedGiType{GiType{"foo", 0}, GiType{"title", 1},
                                   GiType{"args", 1}, GiType{"type", 1}}));
  }

  SECTION("Only siblings")
  {
    std::vector<std::string> gis;
    node_traverse(nd, [&gis](const Node* nd, int depth) {
      gis.emplace_back(nd->gi());
      return TraverseRecursion::k_continue;
    });

    REQUIRE(gis == (std::vector<std::string>{"foo"}));
  }

  SECTION("Mixed recursion/siblings")
  {
    std::vector<std::string> gis;
    node_traverse(nd, [&gis](const Node* nd, int depth) {
      gis.emplace_back(nd->gi());
      if (nd->gi() == "foo") {
        return TraverseRecursion::k_recurse;
      }
      else {
        return TraverseRecursion::k_continue;
      }
    });

    REQUIRE(gis == (std::vector<std::string>{"foo", "title", "args", "type"}));
  }

  SECTION("breaks")
  {
    std::vector<std::string> gis;
    node_traverse(nd, [&gis](const Node* nd, int depth) {
      gis.emplace_back(nd->gi());
      if (nd->gi() == "foo") {
        return TraverseRecursion::k_recurse;
      }
      else if (nd->gi() == "args") {
        return TraverseRecursion::k_break;
      }
      else {
        return TraverseRecursion::k_continue;
      }
    });

    REQUIRE(gis == (std::vector<std::string>{"foo", "title", "args"}));
  }
}


TEST_CASE("Serialize", "[nodes][serialize]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");
  nd->set_property("name", "bar");
  nd->set_property("size", 42);

  auto* type = grove.make_elt_node("type");
  type->set_property("const?", true);
  type->add_child_node(grove.make_text_node("text with \"quots\" and\nnewline"));

  auto* args = grove.make_elt_node("args");
  args->add_node("params", grove.make_elt_node("p1"));
  args->add_node("params", grove.make_elt_node("p2"));
  args->add_node("params", grove.make_elt_node("p3"));

  nd->add_child_node(grove.make_elt_node("title"));
  nd->add_child_node(args);
  nd->add_node("types", type);

  const std::string exptected_output =
    "{ \"type\": \"element\",\n"
    "  \"gi\": \"foo\",\n"
    "  \"children\": [\n"
    "    { \"type\": \"element\",\n"
    "      \"gi\": \"title\"\n"
    "    },\n"
    "    { \"type\": \"element\",\n"
    "      \"gi\": \"args\",\n"
    "      \"params\": [\n"
    "        { \"type\": \"element\",\n"
    "          \"gi\": \"p1\"\n"
    "        },\n"
    "        { \"type\": \"element\",\n"
    "          \"gi\": \"p2\"\n"
    "        },\n"
    "        { \"type\": \"element\",\n"
    "          \"gi\": \"p3\"\n"
    "        }\n"
    "      ]\n"
    "    }\n"
    "  ],\n"
    "  \"name\": \"bar\",\n"
    "  \"size\": 42,\n"
    "  \"types\": [\n"
    "    { \"type\": \"element\",\n"
    "      \"gi\": \"type\",\n"
    "      \"children\": [\n"
    "        { \"type\": \"text\",\n"
    "          \"data\": \"text with \\\"quots\\\" and\\nnewline\"\n"
    "        }\n"
    "      ],\n"
    "      \"const?\": 1\n"
    "    }\n"
    "  ]\n"
    "}\n";

  std::stringstream ss;
  serialize(ss, grove.root_node());
  REQUIRE(ss.str() == exptected_output);
}


TEST_CASE("Serialize without pretty printing", "[nodes][serialize]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");
  nd->set_property("name", "bar");
  nd->set_property("size", 42);

  auto* type = grove.make_elt_node("type");
  type->set_property("const?", true);
  type->add_child_node(grove.make_text_node("text with \"quots\" and\nnewline"));

  auto* args = grove.make_elt_node("args");
  args->add_node("params", grove.make_elt_node("p1"));
  args->add_node("params", grove.make_elt_node("p2"));
  args->add_node("params", grove.make_elt_node("p3"));

  nd->add_child_node(grove.make_elt_node("title"));
  nd->add_child_node(args);
  nd->add_node("types", type);

  const std::string exptected_output =
    "{\"type\":\"element\","
    "\"gi\":\"foo\","
    "\"children\":["
    "{\"type\":\"element\","
    "\"gi\":\"title\""
    "},"
    "{\"type\":\"element\","
    "\"gi\":\"args\","
    "\"params\":["
    "{\"type\":\"element\","
    "\"gi\":\"p1\""
    "},"
    "{\"type\":\"element\","
    "\"gi\":\"p2\""
    "},"
    "{\"type\":\"element\","
    "\"gi\":\"p3\""
    "}"
    "]"
    "}"
    "],"
    "\"name\":\"bar\","
    "\"size\":42,"
    "\"types\":["
    "{\"type\":\"element\","
    "\"gi\":\"type\","
    "\"children\":["
    "{\"type\":\"text\","
    "\"data\":\"text with \\\"quots\\\" and\\nnewline\""
    "}"
    "],"
    "\"const?\":1"
    "}"
    "]"
    "}";

  std::stringstream ss;
  serialize(ss, grove.root_node(), false);
  REQUIRE(ss.str() == exptected_output);
}
