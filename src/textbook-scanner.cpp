// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "textbook-scanner.hpp"
#include "nodes.hpp"
#include "nodeclass.hpp"
#include "textbook-parser.hpp"
#include "textbook-model.hpp"

#include <boost/filesystem.hpp>

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>


namespace eyestep {

namespace fs = boost::filesystem;

//----------------------------------------------------------------------------------------

std::string TextbookScanner::scanner_id() const
{
  return "textbook";
}

std::unordered_set<std::string> TextbookScanner::supported_extensions() const
{
  return {".tb", ".textbook"};
}

Node* TextbookScanner::scan_file(eyestep::Grove& grove, const fs::path& srcfile,
                                 const std::vector<std::string>& incls,
                                 const std::vector<std::string>& defs)
{
  Node* doc_node = grove.make_node(document_class_definition());

  doc_node->set_property(CommonProps::k_source, srcfile.string());
  doc_node->set_property("app-info", "textbook");

  textbook::GroveBuilder grove_builder(doc_node);
  textbook::VariableEnv vars;
  textbook::Catalog catalog;
  auto parser = textbook::Parser(grove, grove_builder, vars, catalog,
                                 nullptr, // docspec
                                 false,   // mixed content
                                 false    // verbose
                                 );

  parser.parse_file(srcfile);

  return doc_node;
}

} // ns eyestep
