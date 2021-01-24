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
#include "tool-setup.hpp"
#include "utils.hpp"

#include "processor-setup.hpp"

#include "cxxopts.hpp"
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

namespace {

std::string to_iso_timestring(std::chrono::system_clock::time_point tp) {
  auto t = std::chrono::system_clock::to_time_t(tp);

  char mbstr[128];
  std::strftime(mbstr, sizeof(mbstr), "%FT%T%z", std::localtime(&t));
  return {mbstr};
}


eyestep::Grove scan_sources(const std::vector<fs::path>& sources,
                            const eyestep::ToolSetup& setup,
                            const cxxopts::ParseResult& args) {
  auto grove = eyestep::Grove{};
  auto* root = grove.set_root_node(eyestep::root_class_definition());

  root->set_property("start-time", to_iso_timestring(std::chrono::system_clock::now()));

  for (const auto& src : sources) {
    std::cout << "Scan " << src << " ...";
    std::cout.flush();

    auto scanner = eyestep::make_scanner_for_file(src, setup, args);

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
} // namespace


int main(int argc, char** argv) {
  try {
    std::string prog_name(argv[0]);
    cxxopts::Options options_decl(prog_name, " <inputs> - textbook processor");

    auto all_scanners = eyestep::utils::join(eyestep::all_scanners(), ", ");
    auto all_procs = eyestep::utils::join(eyestep::all_processors(), ", ");

    // clang-format off
    options_decl.add_options()
      ("h,help",         "produce help message and exit")
      ("version",        "print version and exit")
      ("v,verbose",      "being verbose")
      ("debug",          "being very verbose printing a lot of internal details (for debugging)")
      ("o,output",       "write result to arg",
                         cxxopts::value<std::string>())
      ("b,backend",      std::string("use backend processor [") + all_procs + "]",
                         cxxopts::value<std::string>()->default_value("auto"))
      ("t,template",     "use template",
                         cxxopts::value<std::string>()->default_value("auto"))
      ("D,define",       "Set the variable or call toplevel "
                         "setting <arg>.  If <arg> has the form KEY=VALUE the top-level variable "
                         "%KEY% is set to VALUE.  If it has the form NAME then the top-level "
                         "function (enable-name) will be called.",
                         cxxopts::value<std::vector<std::string>>())
      ("textbook-prefix", "",
                          cxxopts::value<std::string>()->default_value(textbook_prefix()))
      ("input-file",     "input file(s)", cxxopts::value<std::vector<std::string>>())
      ;
    // clang-format on

    eyestep::add_scanner_options(options_decl);
    eyestep::add_processor_options(options_decl);

    auto options = cxxopts::ParseResult{};
    try {
      options_decl.parse_positional("input-file");
      options = options_decl.parse(argc, argv);
    }
    catch (const std::exception& opt) {
      std::cout << "ERROR: " << opt.what() << std::endl;
      return 1;
    }

    if (options.count("help")) {
      std::cout << options_decl.help() << std::endl;
      return 1;
    }
    if (options.count("version")) {
      std::cout << prog_name << " - vr. " << TEXTBOOK_VERSION << std::endl;
      std::cout << "Copyright (c) " << TEXTBOOK_COPYRIGHT_YEAR << ", "
                << TEXTBOOK_COPYRIGHT_OWNER << std::endl;
      return 1;
    }

    auto outf =
      options.count("output") ? options["output"].as<std::string>() : std::string{};
    auto templ_path = options.count("template")
                        ? fs::path(options["template"].as<std::string>())
                        : fs::u8path("auto");
    auto prefix_path = options.count("textbook-prefix")
                         ? options["textbook-prefix"].as<std::string>()
                         : textbook_prefix();
    auto backend = options.count("backend") ? options["backend"].as<std::string>()
                                            : std::string("auto");

    if (options.count("verbose")) {
      std::cout << "outf           : " << outf << std::endl;
      std::cout << "prefix path    : " << prefix_path << std::endl;
      std::cout << "templ_path     : " << templ_path << std::endl;
      std::cout << "all processors : " << all_procs << std::endl;
      std::cout << "all scanners   : " << all_scanners << std::endl;
    }

    auto tool_setup = eyestep::ToolSetup{};
    tool_setup._prefix_path = eyestep::utils::split_paths(prefix_path);

    auto sources = std::vector<fs::path>{};
    if (options.count("input-file")) {
      for (const auto& input : options["input-file"].as<std::vector<std::string>>()) {
        sources.emplace_back(input);
      }
    }

    auto grove = scan_sources(sources, tool_setup, options);
    if (options.count("debug")) {
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

      if (options.count("verbose"))
        std::cout << "use template  : " << eff_templ_path << std::endl;

      auto processor = eyestep::make_processor(backend, options);
      if (processor) {
        processor->set_output_file(
          deduce_output_file(outf, sources, processor->default_output_extension()));

        auto engine =
          eyestep::StyleEngine(prefix_path, backend, options.count("verbose") > 0);
        if (engine.load_style(eff_templ_path)) {
          if (options.count("define")) {
            engine.define_variables(options["define"].as<std::vector<std::string>>());
          }
          auto sosofo = engine.process_node(grove.root_node());

          processor->render_processed_node(&engine, sosofo.get());
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
