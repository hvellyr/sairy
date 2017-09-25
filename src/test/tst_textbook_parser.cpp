// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "../nodeclass.hpp"
#include "../nodelist.hpp"
#include "../nodes.hpp"
#include "../nodeutils.hpp"
#include "../textbook-model.hpp"
#include "../textbook-parser.hpp"

#include "catch/catch.hpp"

#include <boost/filesystem.hpp>

#include <functional>
#include <iostream>
#include <sstream>
#include <string>


using namespace eyestep;
namespace fs = boost::filesystem;

namespace {
Node* with_parser_scan(Grove& grove,
                       const std::function<void(textbook::Parser&)>& proc)
{
  Node* doc_node = grove.make_node(document_class_definition());

  std::vector<fs::path> _catalog_path;
  _catalog_path.push_back("share/textbook/spec");

  textbook::GroveBuilder grove_builder(doc_node);
  textbook::VariableEnv vars;
  textbook::Catalog catalog;
  textbook::Parser parser(grove, grove_builder, vars, catalog,
                          nullptr, // docspec
                          _catalog_path,
                          false, // mixed content
                          false  // verbose
                          );

  proc(parser);

  return doc_node;
}

} // anon ns


TEST_CASE("Basic textbook parsing works", "[textbook][parser]")
{
  Grove grove;
  auto* nd = with_parser_scan(grove, [](textbook::Parser& parser) {
    parser.parse_string("@doc Hello world @end{doc}");
  });

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
