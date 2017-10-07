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

#include "fspp/filesystem.hpp"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>


namespace eyestep {

namespace fs = filesystem;

//----------------------------------------------------------------------------------------

TextbookScanner::TextbookScanner() = default;


TextbookScanner::TextbookScanner(const program_options::variables_map& args)
  : _debug(false) {
  if (!args.empty()) {
    _prefix_path = utils::split_paths(args["textbook-prefix"].as<std::string>());

    transform(begin(_prefix_path), end(_prefix_path), back_inserter(_catalog_path),
              [](const fs::path& path) { return path / "spec"; });

    _debug = args.count("debug") != 0;
  }
}


program_options::options_description TextbookScanner::program_options() const {
  namespace po = program_options;

  auto opts_title = std::string("Textbook parser [selector: '") + scanner_id() + "']";
  auto desc = po::options_description(opts_title);
  return desc;
}


Node* TextbookScanner::scan_file(eyestep::Grove& grove, const fs::path& srcfile) {
  auto* doc_node = grove.make_node(document_class_definition());

  doc_node->set_property(CommonProps::k_source, srcfile.string());
  doc_node->set_property("app-info", "textbook");

  auto grove_builder = textbook::GroveBuilder(doc_node);
  auto vars = textbook::VariableEnv{};
  auto catalog = textbook::Catalog{};
  auto parser = textbook::Parser(grove, grove_builder, vars, catalog,
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
