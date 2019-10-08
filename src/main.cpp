// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "config.hpp"
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
#include <iostream>
#include <memory>
#include <string>


#if !defined(TEXTBOOK_DEFAULT_PREFIX)
#define TEXTBOOK_DEFAULT_PREFIX "/usr/local/share/textbook"
#endif


namespace fs = eyestep::filesystem;
namespace po = program_options;

namespace {

std::string to_iso_timestring(std::chrono::system_clock::time_point tp) {
  auto t = std::chrono::system_clock::to_time_t(tp);

  char mbstr[128];
  std::strftime(mbstr, sizeof(mbstr), "%FT%T%z", std::localtime(&t));
  return {mbstr};
}


eyestep::Grove scan_sources(const std::vector<fs::path>& sources,
                            const po::variables_map& args) {
  auto grove = eyestep::Grove{};
  auto* root = grove.set_root_node(eyestep::root_class_definition());

  root->set_property("start-time", to_iso_timestring(std::chrono::system_clock::now()));

  for (const auto& src : sources) {
    std::cout << "Scan " << src << " ...";
    std::cout.flush();

    auto scanner = eyestep::make_scanner_for_file(src, args);

    if (scanner) {
      auto* nd = scanner->scan_file(grove, src);
      std::cout << " ok" << std::endl;

      root->add_child_node(nd);
    }
    else {
      std::cout << " no scanner for filetype" << std::endl;
    }
  }

  root->set_property("end-time", to_iso_timestring(std::chrono::system_clock::now()));

  return grove;
}


fs::path deduce_output_file(const std::string& outf, const std::vector<fs::path>& sources,
                            const std::string& default_ext) {
  if (!outf.empty()) {
    return outf;
  }

  if (!sources.empty()) {
    auto first = sources[0];
    return first.replace_extension(default_ext).filename();
  }

  return {};
}


std::vector<std::string> document_root_elements_gi(eyestep::Grove& grove) {
  using namespace eyestep;

  auto gis = std::vector<std::string>{};

  auto root_children = grove.root_node()->property<Nodes>(CommonProps::k_children);
  if (root_children.size() >= 1) {
    auto document = root_children[0];
    auto top_elements = document->property<Nodes>(CommonProps::k_children);

    for (const auto nd : top_elements) {
      gis.emplace_back(nd->gi());
    }
  }

  return gis;
}


fs::path look_for_tstyle_file(const std::string& prefix_path,
                              const std::string& style_file) {
  using namespace eyestep;

  for (const auto& path : utils::split_paths(prefix_path)) {
    auto p = path / "tstyle" / style_file;

    std::error_code ec;
    if (fs::is_regular_file(p, ec))
      return p;
  }

  return {};
}


fs::path deduce_templ_from_document(eyestep::Grove& grove, const std::string& prefix_path,
                                    const std::string& backend) {
  using namespace eyestep;

  auto gis = document_root_elements_gi(grove);
  if (!gis.empty()) {
    auto style_file = fs::path(gis[0]).replace_extension(".tstyle");

    auto p =
      look_for_tstyle_file(prefix_path,
                           fs::path(gis[0] + "-" + backend).replace_extension(".tstyle"));
    if (p.empty()) {
      p =
        look_for_tstyle_file(prefix_path, fs::path(gis[0]).replace_extension(".tstyle"));
    }

    return p;
  }

  return {};
}


std::string textbook_prefix() {
  if (auto opt = std::getenv("TEXTBOOK_PREFIX"))
    return {opt};

  return TEXTBOOK_DEFAULT_PREFIX;
}
} // anon namespace


int main(int argc, char** argv) {
  try {
    // Declare the supported options.
    auto desc = po::options_description("");

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
      ("define,D",       po::value<std::vector<std::string>>(), "Set the variable or call toplevel "
                         "setting <arg>.  If <arg> has the form KEY=VALUE the top-level variable "
                         "%KEY% is set to VALUE.  If it has the form NAME then the top-level "
                         "function (enable-name) will be called.")
      ;

    auto hidden = po::options_description("Hidden options");
    hidden.add_options()
      ("textbook-prefix",   po::value<std::string>()->default_value(textbook_prefix()), "")
      ;
    // clang-format on

    auto all_options = po::options_description("All");
    auto visible_options = po::options_description("");

    all_options.add(desc).add(hidden);
    visible_options.add(desc);

    all_options.add(eyestep::scanner_options());
    visible_options.add(eyestep::scanner_options());

    all_options.add(eyestep::processor_options());
    visible_options.add(eyestep::processor_options());

    auto pos_options = po::positional_options_description{};
    pos_options.add("input-file", -1);

    auto vm = po::variables_map{};
    po::store(po::parse_command_line(argc, argv, all_options, pos_options), vm);
    po::notify(vm);

    if (vm.count("help")) {
      std::cout << visible_options << "\n";
      exit(1);
    }

    auto outf = vm["output"].as<std::string>();
    auto templ_path = fs::path(vm["template"].as<std::string>());
    auto prefix_path = vm["textbook-prefix"].as<std::string>();
    auto backend = vm["backend"].as<std::string>();

    if (vm.count("verbose")) {
      std::cout << "outf        : " << outf << std::endl;
      std::cout << "prefix path : " << prefix_path << std::endl;
      std::cout << "templ_path  : " << templ_path << std::endl;
    }

    auto sources = std::vector<fs::path>{};
    if (vm.count("input-file")) {
      for (const auto& input : vm["input-file"].as<std::vector<std::string>>()) {
        sources.emplace_back(input);
      }
    }

    auto grove = scan_sources(sources, vm);
    if (vm.count("debug")) {
      eyestep::serialize(std::cout, grove.root_node());
    }

    if (!templ_path.empty()) {
      auto eff_templ_path = templ_path == "auto"
                              ? deduce_templ_from_document(grove, prefix_path, backend)
                              : templ_path;
      if (eff_templ_path.empty()) {
        std::cerr << "No stylesheet found" << std::endl;
        exit(1);
      }

      if (vm.count("verbose"))
        std::cout << "use template  : " << eff_templ_path << std::endl;

      auto processor = eyestep::make_processor(backend, vm);
      if (processor) {
        processor->set_output_file(
          deduce_output_file(outf, sources, processor->default_output_extension()));

        auto engine = eyestep::StyleEngine(prefix_path, backend);
        if (engine.load_style(eff_templ_path)) {
          if (vm.count("define")) {
            engine.define_variables(vm["define"].as<std::vector<std::string>>());
          }
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
