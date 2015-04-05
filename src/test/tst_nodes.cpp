// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "catch/catch.hpp"

#include "../nodeclass.hpp"
#include "../nodes.hpp"

#include <boost/range/irange.hpp>

#include <string>
#include <sstream>
#include <tuple>


using namespace eyestep;


TEST_CASE("Base node creation", "[nodes]")
{
  Node nd(element_class_definition());
  REQUIRE(nd.classname() == "element");
}


TEST_CASE("Default node as no gi", "[nodes]")
{
  Node nd;
  REQUIRE(nd.gi().empty());
}


TEST_CASE("Set properties", "[nodes]")
{
  Node nd;
  nd.set_property("name", "foo");
  nd.set_property("size", 42);

  REQUIRE(nd.property<std::string>("name") == "foo");
  REQUIRE(nd.property<int>("size") == 42);
}


TEST_CASE("Unknown properties report as default", "[nodes]")
{
  Node nd;

  REQUIRE(nd.property<std::string>("name").empty());
  REQUIRE(!nd.has_property("name"));
}


TEST_CASE("Add node", "[nodes]")
{
  Grove grove;
  Node* nd = grove.make_node(document_class_definition());

  nd->add_node("kids", grove.make_elt_node("foo"));
  nd->add_node("kids", grove.make_elt_node("bar"));

  REQUIRE(nd->property<Nodes>("kids").size() == 2u);
  REQUIRE(nd->property<Nodes>("kids")[0]->gi() == "foo");
  REQUIRE(nd->property<Nodes>("kids")[1]->gi() == "bar");
}


TEST_CASE("Parent property", "[nodes]")
{
  Grove grove;
  auto* a = grove.set_root_node(root_class_definition());
  auto* b = grove.make_elt_node("b");

  a->add_child_node(b);

  REQUIRE(b->gi() == "b");

  REQUIRE(b->property<Node*>("parent")->classname() == "root");
  REQUIRE(b->parent()->classname() == "root");
  REQUIRE(b->parent() == a);
}


TEST_CASE("Traverse", "[nodes]")
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


TEST_CASE("Serialize", "[nodes]")
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
  nd->add_node("types", type);

  const std::string exptected_output =
    "<element gi='foo'>\n"
    "  <prop nm='name'>bar</prop>\n"
    "  <prop nm='size'>42</prop>\n"
    "  <prop nm='types'>\n"
    "    <element gi='type'>\n"
    "      <prop nm='const?'>1</prop>\n"
    "    </element>\n"
    "  </prop>\n"
    "  <element gi='title'>\n"
    "  </element>\n"
    "  <element gi='args'>\n"
    "    <prop nm='params'>\n"
    "      <element gi='p1'>\n"
    "      </element>\n"
    "      <element gi='p2'>\n"
    "      </element>\n"
    "      <element gi='p3'>\n"
    "      </element>\n"
    "    </prop>\n"
    "  </element>\n"
    "</element>\n";

  std::stringstream ss;
  serialize(ss, grove.root_node());
  REQUIRE(ss.str() == exptected_output);
}


TEST_CASE("Attributes add text nodes", "[nodes, attributes]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");
  nd->add_attribute("gaz", "fieps");

  REQUIRE(nd->attributes().size() == 1);
  REQUIRE(nd->attributes()[0]->classname() == "text");
  REQUIRE(nd->attributes()[0]->property<std::string>(CommonProps::k_attr_name) == "gaz");
  REQUIRE(nd->attributes()[0]->property<std::string>(CommonProps::k_data) == "fieps");
}


TEST_CASE("Attributes add nodes", "[nodes, attributes]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");

  auto* boo = grove.make_elt_node("boo");
  nd->add_attribute("gaz", boo);

  REQUIRE(nd->attributes().size() == 1);
  REQUIRE(nd->attributes()[0]->classname() == "element");
  REQUIRE(nd->attributes()[0]->property<std::string>(CommonProps::k_attr_name) == "gaz");
  REQUIRE(nd->attributes()[0]->property<std::string>(CommonProps::k_gi) == "boo");
}


TEST_CASE("Attributes add nodelist", "[nodes, attributes]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");

  const auto texts = std::vector<std::string>{ "hello", " ", "world!" };

  Nodes nodes;
  for (const auto txt : texts) {
    nodes.push_back(grove.make_text_node(txt));
  }
  nd->add_attribute("gaz", nodes);

  REQUIRE(nd->attributes().size() == texts.size());
  for (auto i : boost::irange(0, int(texts.size()))) {
    REQUIRE(nd->attributes()[i]->classname() == "text");
    REQUIRE(nd->attributes()[i]->property<std::string>(CommonProps::k_attr_name) == "gaz");
    REQUIRE(nd->attributes()[i]->property<std::string>(CommonProps::k_data) == texts[i]);
  }
}


TEST_CASE("Attributes setting attributes removes previous attributes", "[nodes, attributes]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");
  nd->add_attribute("gaz", "fieps");

  REQUIRE(nd->attributes().size() == 1);

  const auto texts = std::vector<std::string>{ "hello", " ", "world!" };
  const auto attrnms = std::vector<std::string>{ "a", "b", "c" };

  Nodes nodes;
  int ai = 0;
  for (const auto txt : texts) {
    auto* txtnd = grove.make_text_node(txt);
    txtnd->set_property(CommonProps::k_attr_name, attrnms[ai++]);
    nodes.push_back(txtnd);
  }
  nd->set_attributes(nodes);

  REQUIRE(nd->attributes().size() == texts.size());
  for (auto i : boost::irange(0, int(texts.size()))) {
    REQUIRE(nd->attributes()[i]->classname() == "text");
    REQUIRE(nd->attributes()[i]->property<std::string>(CommonProps::k_attr_name) == attrnms[i]);
    REQUIRE(nd->attributes()[i]->property<std::string>(CommonProps::k_data) == texts[i]);
  }
}
