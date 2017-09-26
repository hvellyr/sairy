// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "textbook-scanner.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"
#include "nodeutils.hpp"
#include "textbook-model.hpp"
#include "textbook-parser.hpp"
#include "utils.hpp"

#include "program_options/program_options.hpp"

#include <boost/range/iterator.hpp>
#include <boost/range/iterator_range.hpp>
#include <boost/range/adaptor/transformed.hpp>
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

TextbookScanner::TextbookScanner() : _debug(false)
{
}

TextbookScanner::TextbookScanner(
  const program_options::variables_map& args)
  : _debug(false)
{
  if (!args.empty()) {
    _prefix_path = utils::split_paths(args["textbook-prefix"].as<std::string>());

    _catalog_path = boost::copy_range<std::vector<fs::path>>(
      _prefix_path | boost::adaptors::transformed([](const fs::path& path) {
        return path / "spec";
      }));

    _debug = args.count("debug") != 0;
  }
}

std::string TextbookScanner::scanner_id() const
{
  return "textbook";
}

std::unordered_set<std::string> TextbookScanner::supported_extensions() const
{
  return {".tb", ".textbook"};
}

program_options::options_description TextbookScanner::program_options() const
{
  namespace po = program_options;

  std::string opts_title =
    std::string("Textbook parser [selector: '") + scanner_id() + "']";
  po::options_description desc(opts_title);
  return desc;
}

Node* TextbookScanner::scan_file(eyestep::Grove& grove, const fs::path& srcfile)
{
  Node* doc_node = grove.make_node(document_class_definition());

  doc_node->set_property(CommonProps::k_source, srcfile.string());
  doc_node->set_property("app-info", "textbook");

  textbook::GroveBuilder grove_builder(doc_node);
  textbook::VariableEnv vars;
  textbook::Catalog catalog;
  textbook::Parser parser(grove, grove_builder, vars, catalog,
                          nullptr, // docspec
                          _catalog_path,
                          false, // mixed content
                          false  // verbose
                          );

  parser.parse_file(srcfile);

  if (_debug) {
    serialize(std::cerr, doc_node);
  }

  return doc_node;
}

} // ns eyestep
