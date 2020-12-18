// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "../nodeclass.hpp"
#include "../nodelist.hpp"
#include "../nodes.hpp"
#include "../nodeutils.hpp"
#include "../textbook-model.hpp"
#include "../textbook-parser.hpp"

#include "program_options/program_options.hpp"

#include "fspp/filesystem.hpp"
#include "fspp/utils.hpp"

#include "json.hpp"

#include <functional>
#include <iostream>
#include <sstream>
#include <string>


using namespace eyestep;
namespace fs = filesystem;
namespace po = program_options;

using json = nlohmann::json;


namespace {
Node* with_parser_scan(Grove& grove, const std::function<void(textbook::Parser&)>& proc) {
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

json reparse_node_as_json(Node* nd) {
  std::stringstream buf;
  serialize(buf, nd, false);

  return json::parse(buf.str());
}

void serialize_node_if_missing(Node* nd, const fs::path& path,
                               const po::variables_map& vm) {
  if (vm.count("serialize")) {
    serialize(std::cout, nd, true);
  }
}

bool test_file(const fs::path& path, const po::variables_map& vm) {
  if (path.extension() == ".textbook" && fs::is_regular_file(path)) {
    std::cerr << "Testing input file " << path << " ...";
    std::cerr.flush();

    Grove grove;
    Node* nd = nullptr;

    try {
      nd = with_parser_scan(grove, [&path](textbook::Parser& parser) {
        parser.parse_file(path);
      });
      if (nd == nullptr) {
        std::cerr << "FAILED" << std::endl
                  << "    parsing of source file failed" << std::endl;
        return false;
      }
    }
    catch (const eyestep::textbook::ParseException& e) {
      std::cerr << "FAILED" << std::endl
                << "    parsing of source file failed: " << e.what() << std::endl;
      return false;
    }
    catch (const std::exception& e) {
      std::cerr << "FAILED" << std::endl
                << "    parsing of source file failed: " << e.what() << std::endl;
      return false;
    }


    auto excp_path = path;
    excp_path.replace_extension(".js");

    if (fs::exists(excp_path)) {
      auto file = fs::File(excp_path);
      auto& inp = file.open(std::ios::in | std::ios::binary);

      auto expected_root_elt = json::parse(inp);

      auto parsed_json = reparse_node_as_json(nd);

      if (expected_root_elt == parsed_json) {
        std::cerr << "ok" << std::endl;
        serialize_node_if_missing(nd, path, vm);
        return true;
      }

      std::cerr << "FAILED" << std::endl
                << "    parsed and expected outcome differ" << std::endl;
      if (vm.count("verbose")) {
        std::cerr << "EXPECTED:" << std::endl;
        std::cerr << std::setw(4) << expected_root_elt;
        std::cerr << std::endl << "ACTUAL:" << std::endl;
        std::cerr << std::setw(4) << parsed_json;
        std::cerr << std::endl;
      }
      return false;
    }
    else {
      std::cerr << "parsed" << std::endl;
      serialize_node_if_missing(nd, path, vm);
      return true;
    }
  }

  return true;
}

int test_in_dir(const fs::path& path, const po::variables_map& vm) {
  int result = 0;

  std::vector<fs::directory_entry> dirents;
  std::copy(fs::directory_iterator(path), fs::directory_iterator(),
            std::back_inserter(dirents));
  for (const auto& dirent : dirents) {
    if (dirent.status().type() == fs::file_type::regular) {
      if (!test_file(dirent.path(), vm)) {
        result = 1;
      }
    }
  }

  return result;
}

int run_tests(const std::vector<fs::path>& sources, const po::variables_map& vm) {
  int result = 0;

  for (const auto& path : sources) {
    if (fs::is_directory(path)) {
      auto r = test_in_dir(path, vm);
      if (r != 0) {
        result = r;
      }
    }
    else if (fs::is_regular_file(path)) {
      if (!test_file(path, vm)) {
        result = 1;
      }
    }
  }

  return result;
}

} // anon ns


int main(int argc, char** argv) {
  po::options_description all_options("Options");

  // clang-format off
  all_options.add_options()
    ("help,h",         "produce help message")
    ("verbose,v",      "being verbose")
    ("serialize,S",    "parse & print json to stdout")
    ("input-file",     po::value<std::vector<std::string>>(),
                       "input file")
    ;
  // clang-format on

  po::positional_options_description pos_options;
  pos_options.add("input-file", -1);

  po::variables_map vm;
  po::store(po::parse_command_line(argc, argv, all_options, pos_options), vm);
  po::notify(vm);

  if (vm.count("help")) {
    std::cout << all_options << std::endl;
    exit(1);
  }

  std::vector<fs::path> sources;
  if (vm.count("input-file")) {
    for (const auto& input : vm["input-file"].as<std::vector<std::string>>()) {
      sources.emplace_back(input);
    }
  }
  else {
    std::cout << all_options << std::endl;
    exit(1);
  }

  return run_tests(sources, vm);
}
