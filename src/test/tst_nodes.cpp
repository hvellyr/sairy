// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "catch/catch.hpp"

#include "../nodeclass.hpp"
#include "../nodes.hpp"
#include "../nodeutils.hpp"

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


TEST_CASE("Attributes add text nodes", "[nodes, attributes]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");
  nd->add_attribute("gaz", "fieps");

  REQUIRE(nd->attributes().size() == 1);
  REQUIRE(nd->attributes()[0]->classname() == "text");
  REQUIRE(nd->attributes()[0]->property<std::string>(
            CommonProps::k_attr_name) == "gaz");
  REQUIRE(nd->attributes()[0]->property<std::string>(CommonProps::k_data) ==
          "fieps");

  REQUIRE(nd->has_attribute("gaz"));
  REQUIRE(!nd->has_attribute("boo"));
}


TEST_CASE("Attributes add nodes", "[nodes, attributes]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");

  auto* boo = grove.make_elt_node("boo");
  nd->add_attribute("gaz", boo);

  REQUIRE(nd->attributes().size() == 1);
  REQUIRE(nd->attributes()[0]->classname() == "element");
  REQUIRE(nd->attributes()[0]->property<std::string>(
            CommonProps::k_attr_name) == "gaz");
  REQUIRE(nd->attributes()[0]->property<std::string>(CommonProps::k_gi) ==
          "boo");

  REQUIRE(nd->has_attribute("gaz"));
  REQUIRE(!nd->has_attribute("boo"));
}


TEST_CASE("Attributes add nodelist", "[nodes, attributes]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");

  const auto texts = std::vector<std::string>{"hello", " ", "world!"};

  Nodes nodes;
  for (const auto txt : texts) {
    nodes.push_back(grove.make_text_node(txt));
  }
  nd->add_attribute("gaz", nodes);

  REQUIRE(nd->attributes().size() == texts.size());
  for (auto i : boost::irange(0, int(texts.size()))) {
    REQUIRE(nd->attributes()[i]->classname() == "text");
    REQUIRE(nd->attributes()[i]->property<std::string>(
              CommonProps::k_attr_name) == "gaz");
    REQUIRE(nd->attributes()[i]->property<std::string>(CommonProps::k_data) ==
            texts[i]);
  }

  REQUIRE(nd->has_attribute("gaz"));
  REQUIRE(!nd->has_attribute("boo"));
}


TEST_CASE("Attributes setting attributes removes previous attributes", "[nodes, attributes]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");
  nd->add_attribute("gaz", "fieps");

  REQUIRE(nd->attributes().size() == 1);
  REQUIRE(nd->has_attribute("gaz"));

  const auto texts = std::vector<std::string>{"hello", " ", "world!"};
  const auto attrnms = std::vector<std::string>{"a", "b", "c"};

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
    REQUIRE(nd->attributes()[i]->property<std::string>(
              CommonProps::k_attr_name) == attrnms[i]);
    REQUIRE(nd->attributes()[i]->property<std::string>(CommonProps::k_data) ==
            texts[i]);
  }

  REQUIRE(!nd->has_attribute("gaz"));
  REQUIRE(nd->has_attribute("a"));
  REQUIRE(nd->has_attribute("b"));
  REQUIRE(nd->has_attribute("c"));
}


TEST_CASE("Attributes access", "[nodes, attributes]")
{
  Grove grove;

  auto* nd = grove.make_elt_node("foo");

  const auto texts = std::vector<std::string>{"hello", " ", "world!"};
  const auto attrnms = std::vector<std::string>{"a", "b", "c"};

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
    auto* n2 = nd->attribute(attrnms[i]);
    REQUIRE(n2 != nullptr);
    REQUIRE(n2->property<std::string>(CommonProps::k_data) == texts[i]);
  }
}
