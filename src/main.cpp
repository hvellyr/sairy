// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"
#include "nodeutils.hpp"
#include "processor.hpp"
#include "scanner-setup.hpp"
#include "scanner.hpp"
#include "scm-context.hpp"
#include "sosofo.hpp"
#include "style-engine.hpp"
#include "utils.hpp"

#include "processor-setup.hpp"

#include "program_options/program_options.hpp"

#include "fspp/filesystem.hpp"

#include <algorithm>
#include <chrono>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>


#define TEXTBOOK_DEFAULT_PREFIX "/usr/local/share/textbook"


namespace fs = eyestep::filesystem;
namespace po = program_options;

namespace {

std::string to_iso_timestring(std::chrono::system_clock::time_point tp)
{
  auto t = std::chrono::system_clock::to_time_t(tp);

  std::stringstream ss;
  ss << std::put_time(std::localtime(&t), "%FT%T%z");
  return ss.str();
}


eyestep::Grove scan_sources(const std::vector<fs::path>& sources,
                            const po::variables_map& args)
{
  eyestep::Grove grove;
  eyestep::Node* root = grove.set_root_node(eyestep::root_class_definition());

  root->set_property("start-time", to_iso_timestring(std::chrono::system_clock::now()));

  if (!sources.empty()) {
    for (const auto& src : sources) {
      std::cout << "Scan " << src << " ...";
      std::cout.flush();

      std::unique_ptr<eyestep::IScanner> scanner =
        eyestep::make_scanner_for_file(src, args);

      if (scanner) {
        eyestep::Node* nd = scanner->scan_file(grove, src);
        std::cout << " ok" << std::endl;

        root->add_child_node(nd);
      }
      else {
        std::cout << " no scanner for filetype" << std::endl;
      }
    }
  }

  root->set_property("end-time", to_iso_timestring(std::chrono::system_clock::now()));

  return grove;
}


fs::path deduce_output_file(const std::string& outf,
                            const std::vector<fs::path>& sources,
                            const std::string& default_ext)
{
  if (!outf.empty()) {
    return outf;
  }

  if (!sources.empty()) {
    auto first = sources[0];
    return first.replace_extension(default_ext).filename();
  }

  return fs::path();
}


std::vector<std::string> document_root_elements_gi(eyestep::Grove& grove)
{
  using namespace eyestep;

  std::vector<std::string> gis;

  auto root_children =
    grove.root_node()->property<Nodes>(CommonProps::k_children);
  if (root_children.size() == 1) {
    auto document = root_children[0];
    auto top_elements = document->property<Nodes>(CommonProps::k_children);
    for (const auto nd : top_elements) {
      gis.emplace_back(nd->gi());
    }
  }

  return gis;
}


fs::path deduce_templ_from_document(eyestep::Grove& grove,
                                    const std::string& prefix_path)
{
  using namespace eyestep;

  auto gis = document_root_elements_gi(grove);
  if (!gis.empty()) {
    auto style_file = fs::path(gis[0]).replace_extension(".tstyle");

    for (const auto& path : utils::split_paths(prefix_path)) {
      auto p = path / "tstyle" / style_file;
      if (fs::exists(p))
        return p;
    }
  }

  return fs::path();
}


std::string textbook_prefix()
{
  if (auto opt = std::getenv("TEXTBOOK_PREFIX")) {
    return std::string(opt);
  }

  return TEXTBOOK_DEFAULT_PREFIX;
}
} // anon namespace


int main(int argc, char** argv)
{
  try {
    // Declare the supported options.
    po::options_description desc("");

    // clang-format off
    desc.add_options()
      ("help,h",         "produce help message")
      ("verbose,v",      "being verbose")
      ("debug",          "being very verbose printing a lot of internal details (for debugging)")
      ("output,o",       po::value<std::string>()->default_value(std::string()),
                         "write result to arg")
      ("backend,b",      po::value<std::string>()->default_value(std::string("auto")),
                         "use backend processor")
      ("template,t",     po::value<std::string>()->default_value("auto"),
                         "use template")
      ("input-file",     po::value<std::vector<std::string>>(),
                         "input file")
      ;

    po::options_description hidden("Hidden options");
    hidden.add_options()
      ("textbook-prefix",   po::value<std::string>()->default_value(textbook_prefix()), "")
      ;
    // clang-format on

    po::options_description all_options("All");
    po::options_description visible_options("");

    all_options.add(desc).add(hidden);
    visible_options.add(desc);

    all_options.add(eyestep::scanner_options());
    visible_options.add(eyestep::scanner_options());

    all_options.add(eyestep::processor_options());
    visible_options.add(eyestep::processor_options());

    po::positional_options_description pos_options;
    pos_options.add("input-file", -1);

    po::variables_map vm;
    po::store(po::parse_command_line(argc, argv, all_options, pos_options), vm);
    po::notify(vm);

    if (vm.count("help")) {
      std::cout << visible_options << "\n";
      exit(1);
    }

    std::string outf = vm["output"].as<std::string>();
    auto templ_path = fs::path(vm["template"].as<std::string>());
    auto prefix_path = vm["textbook-prefix"].as<std::string>();
    std::string backend = vm["backend"].as<std::string>();

    if (vm.count("verbose")) {
      std::cout << "outf        : " << outf << std::endl;
      std::cout << "prefix path : " << prefix_path << std::endl;
      std::cout << "templ_path  : " << templ_path << std::endl;
    }

    std::vector<fs::path> sources;
    if (vm.count("input-file")) {
      for (const auto& input :
           vm["input-file"].as<std::vector<std::string>>()) {
        sources.emplace_back(input);
      }
    }

    eyestep::Grove grove = scan_sources(sources, vm);
    if (vm.count("debug")) {
      eyestep::serialize(std::cout, grove.root_node());
    }

    if (!templ_path.empty()) {
      auto eff_templ_path = templ_path == "auto"
                              ? deduce_templ_from_document(grove, prefix_path)
                              : templ_path;
      if (eff_templ_path.empty()) {
        std::cerr << "No stylesheet found" << std::endl;
        exit(1);
      }

      if (vm.count("verbose"))
        std::cout << "use template  : " << eff_templ_path << std::endl;

      auto processor = eyestep::make_processor_for_file(backend, vm);
      if (processor) {
        processor->set_output_file(
          deduce_output_file(outf, sources,
                             processor->default_output_extension()));

        auto engine = eyestep::StyleEngine(prefix_path, backend);
        if (engine.load_style(eff_templ_path)) {
          auto sosofo = engine.process_node(grove.root_node());

          processor->render_processed_node(sosofo.get());
        }
        else {
          std::cerr << "Loading stylesheet failed" << std::endl;
        }
      }
      else {
        std::cerr << "Unknown backend: " << backend << std::endl;
      }
    }
  }
  catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
  }

  exit(0);
}
