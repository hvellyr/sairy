// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"
#include "processor.hpp"
#include "scanner-setup.hpp"
#include "scanner.hpp"
#include "scm-context.hpp"
#include "sosofo.hpp"
#include "style-engine.hpp"
#include "utils.hpp"

#include "processor-setup.hpp"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <boost/program_options/parsers.hpp>

#include <string>
#include <iostream>
#include <memory>

#define SAIRY_DEFAULT_PREFIX "/usr/local/share/sairy"


namespace fs = boost::filesystem;
using namespace boost::posix_time;
using namespace boost::gregorian;
namespace po = boost::program_options;

namespace {

eyestep::Grove scan_sources(const std::vector<fs::path>& sources,
                            const po::variables_map& args)
{
  eyestep::Grove grove;
  eyestep::Node* root = grove.set_root_node(eyestep::root_class_definition());

  root->set_property("start-time",
                     to_iso_extended_string(microsec_clock::local_time()));

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

  root->set_property("end-time",
                     to_iso_extended_string(microsec_clock::local_time()));

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

} // anon namespace


int main(int argc, char** argv)
{
  try {
    // Declare the supported options.
    po::options_description desc("Allowed options");

    // clang-format off
    desc.add_options()
      ("help,h",         po::bool_switch(), "produce help message")
      ("verbose,v",      po::bool_switch(), "being verbose")
      ("debug",          po::bool_switch(),
                         "being very verbose printing a lot of internal details (for debugging)")
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
      ("sairy-prefix",   po::value<std::string>()->default_value(SAIRY_DEFAULT_PREFIX), "")
      ;
    // clang-format on

    po::options_description all_options;
    po::options_description visible_options;

    all_options.add(desc).add(hidden);
    visible_options.add(desc);

    all_options.add(eyestep::scanner_options());
    visible_options.add(eyestep::scanner_options());

    all_options.add(eyestep::processor_options());
    visible_options.add(eyestep::processor_options());

    po::positional_options_description p;
    p.add("input-file", -1);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv)
                .options(all_options)
                .positional(p)
                //.allow_unregistered()
                .run(),
              vm);
    po::store(po::parse_environment(all_options, [](const std::string& opt) {
      if (opt == "SAIRY_PREFIX")
        return std::string("sairy-prefix");
      return std::string();
    }), vm);
    po::notify(vm);

    if (vm["help"].as<bool>()) {
      std::cout << visible_options << "\n";
      exit(1);
    }

    std::string outf = vm["output"].as<std::string>();
    std::string templ_path = vm["template"].as<std::string>();
    std::string prefix_path = vm["sairy-prefix"].as<std::string>();
    std::string backend = vm["backend"].as<std::string>();

    if (vm["verbose"].as<bool>()) {
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
    if (vm["debug"].as<bool>()) {
      eyestep::serialize(std::cout, grove.root_node());
    }

    if (!templ_path.empty()) {
      auto processor = eyestep::make_processor_for_file(backend, vm);
      if (processor) {
        processor->set_output_file(
          deduce_output_file(outf, sources,
                             processor->default_output_extension()));

        eyestep::StyleEngine engine(prefix_path, backend);
        if (engine.load_style(templ_path)) {
          auto sosofo = std::move(engine.process_node(grove.root_node()));

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
