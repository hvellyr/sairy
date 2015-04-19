// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "../nodeclass.hpp"
#include "../nodelist.hpp"
#include "../nodes.hpp"
#include "../nodeutils.hpp"
#include "../textbook-model.hpp"
#include "../textbook-parser.hpp"

#include <boost/filesystem.hpp>

#include <string>
#include <iostream>
#include <sstream>
#include "catch/catch.hpp"


using namespace eyestep;
namespace fs = boost::filesystem;

namespace {
Node* scan_text(Grove& grove, const std::string& text)
{
  Node* doc_node = grove.make_node(document_class_definition());

  std::vector<fs::path> _catalog_path;
  _catalog_path.push_back("share/sairy/textbook/spec");

  textbook::GroveBuilder grove_builder(doc_node);
  textbook::VariableEnv vars;
  textbook::Catalog catalog;
  textbook::Parser parser(grove, grove_builder, vars, catalog,
                          nullptr, // docspec
                          _catalog_path,
                          false, // mixed content
                          false  // verbose
                          );

  parser.parse_string(text);

  return doc_node;
}
} // anon ns


TEST_CASE("Basic textbook parsing works", "[textbook][parser]")
{
  Grove grove;
  auto* nd = scan_text(grove, "@doc Hello world @end{doc}");
  REQUIRE(nd != nullptr);

  std::stringstream buf;
  serialize(buf, nd, false);

  REQUIRE(buf.str() ==
          "{\"type\":\"document\","
          "\"children\":["
          "{\"type\":\"element\","
          "\"gi\":\"doc\","
          "\"attributes\":[],"
          "\"children\":["
          "{\"type\":\"element\","
          "\"gi\":\"p\","
          "\"children\":["
          "{\"type\":\"text\",\"data\":\"Hello world \"}]"
          "}]"
          "}]}");
}
