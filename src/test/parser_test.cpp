// Copyright (C) 2015 Gregor Klinke
// All rights reserved.

#include "../nodeclass.hpp"
#include "../nodelist.hpp"
#include "../nodes.hpp"
#include "../nodeutils.hpp"
#include "../textbook-model.hpp"
#include "../textbook-parser.hpp"
#include "../cpp-scanner.hpp"

#include "json_spirit/json_spirit_value.h"
#include "json_spirit/json_spirit_reader_template.h"
#include "json_spirit/json_spirit_writer_template.h"

#include <boost/algorithm/string/predicate.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/program_options.hpp>
#include <boost/program_options/parsers.hpp>

#include <functional>
#include <iostream>
#include <sstream>
#include <string>


using namespace eyestep;
namespace fs = boost::filesystem;
namespace po = boost::program_options;

namespace {
Node* with_parser_tb_scan(Grove& grove, const po::variables_map& vm,
                          const std::function<void(textbook::Parser&)>& proc)
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
                          vm["verbose"].as<bool>());

  proc(parser);

  return doc_node;
}

json_spirit::Value reparse_node_as_json(Node* nd)
{
  std::stringstream buf;
  serialize(buf, nd, false);

  json_spirit::Value root_elt;
  if (json_spirit::read_string(buf.str(), root_elt)) {
    return root_elt;
  }

  return json_spirit::Value();
}

void serialize_node_if_missing(Node* nd, const fs::path& path,
                               const po::variables_map& vm)
{
  if (vm["serialize"].as<bool>()) {
    serialize(std::cout, nd, true);
  }
}


Node* test_tb_file(const fs::path& path, const po::variables_map& vm,
                   Grove& grove)
{
  Node* nd = nullptr;

  try {
    nd = with_parser_tb_scan(grove, vm, [&path](textbook::Parser& parser) {
      parser.parse_file(path);
    });
    if (nd == nullptr) {
      std::cerr << "FAILED" << std::endl
                << "    parsing of source file failed" << std::endl;
      return nullptr;
    }
  }
  catch (const eyestep::textbook::ParseException& e) {
    std::cerr << "FAILED" << std::endl
              << "    parsing of source file failed: " << e.what() << std::endl;
    return nullptr;
  }
  catch (const std::exception& e) {
    std::cerr << "FAILED" << std::endl
              << "    parsing of source file failed: " << e.what() << std::endl;
    return nullptr;
  }

  return nd;
}


Node* test_cpp_file(const fs::path& path, po::variables_map vm, Grove& grove)
{
  Node* nd = nullptr;

  if (boost::algorithm::starts_with(path.filename().string(),
                                    std::string("cpp11-"))) {
    vm.insert(
      std::make_pair("std", po::variable_value(std::string("c++11"), false)));
  }
  else if (boost::algorithm::starts_with(path.filename().string(),
                                         std::string("cpp14-"))) {
    vm.insert(
      std::make_pair("std", po::variable_value(std::string("c++1y"), false)));
  }

  try {
    CppScanner scanner(vm);
    nd = scanner.scan_file(grove, path);
    if (nd == nullptr) {
      std::cerr << "FAILED" << std::endl
                << "    parsing of source file failed" << std::endl;
      return nullptr;
    }
  }
  catch (const std::exception& e) {
    std::cerr << "FAILED" << std::endl
              << "    parsing of source file failed: " << e.what() << std::endl;
    return nullptr;
  }

  return nd;
}


bool test_file(const fs::path& path, const po::variables_map& vm)
{
  if (fs::is_regular_file(path)) {
    Grove grove;
    Node* nd = nullptr;

    if (path.extension() == ".textbook") {
      std::cerr << "Testing input file " << path << " ...";
      std::cerr.flush();
      nd = test_tb_file(path, vm, grove);
    }
    else if (path.extension() == ".hpp" || path.extension() == ".h") {
      std::cerr << "Testing input file " << path << " ...";
      std::cerr.flush();
      nd = test_cpp_file(path, vm, grove);
    }
    else {
      return true;
    }

    if (!nd) {
      return false;
    }

    auto excp_path = path;
    excp_path.replace_extension(".js");

    if (fs::exists(excp_path)) {
      fs::ifstream inp(excp_path);

      json_spirit::Value expected_root_elt;
      bool reading_expected_file_succeeded =
        json_spirit::read_stream(inp, expected_root_elt);
      if (!reading_expected_file_succeeded) {
        std::cerr << "FAILED" << std::endl
                  << "    failed to read expected json file" << std::endl;
        serialize_node_if_missing(nd, path, vm);
        return false;
      }

      json_spirit::Value parsed_json = reparse_node_as_json(nd);

      if (expected_root_elt == parsed_json) {
        std::cerr << "ok" << std::endl;
        serialize_node_if_missing(nd, path, vm);
        return true;
      }

      serialize_node_if_missing(nd, path, vm);

      std::cerr << "FAILED" << std::endl
                << "    parsed and expected outcome differ" << std::endl;
      if (vm["dev"].as<bool>()) {
        std::cerr << "EXPECTED:" << std::endl;
        json_spirit::write_stream(expected_root_elt, std::cerr, true);
        std::cerr << std::endl
                  << "ACTUAL:" << std::endl;
        json_spirit::write_stream(parsed_json, std::cerr, true);
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

int test_in_dir(const fs::path& path, const po::variables_map& vm)
{
  int result = 0;

  std::vector<fs::directory_entry> dirents;
  std::copy(fs::directory_iterator(path), fs::directory_iterator(),
            std::back_inserter(dirents));
  for (const auto& dirent : dirents) {
    if (dirent.status().type() == fs::regular_file) {
      if (!test_file(dirent.path(), vm)) {
        result = 1;
      }
    }
  }

  return result;
}

int run_tests(const std::vector<fs::path>& sources, const po::variables_map& vm)
{
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

namespace json_spirit {
std::ostream& operator<<(std::ostream& os, const Value& value)
{
  write_stream(value, os, true);
  return os;
}
} // ns json_spirit


int main(int argc, char** argv)
{
  po::options_description all_options("Options");

  // clang-format off
  all_options.add_options()
    ("help,h",         po::bool_switch(), "produce help message")
    ("verbose,v",      po::bool_switch(), "being verbose")
    ("dev",            po::bool_switch(), "development mode (being more verbose)")
    ("serialize,S",    po::bool_switch(), "parse & print json to stdout")
    ("input-file",     po::value<std::vector<std::string>>(),
                       "input file")
    ;
  // clang-format on

  po::positional_options_description p;
  p.add("input-file", -1);

  po::variables_map vm;
  po::store(po::command_line_parser(argc, argv)
              .options(all_options)
              .positional(p)
              .run(),
            vm);
  po::notify(vm);

  if (vm["help"].as<bool>()) {
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
