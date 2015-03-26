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
#include "source.hpp"
#include "style-engine.hpp"
#include "utils.hpp"

#include "debug-processor.hpp"
#include "html-processor.hpp"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/range/adaptor/transformed.hpp>

#include <string>
#include <iostream>
#include <memory>

#define SAIRY_DEFAULT_PREFIX "/usr/local/share/sairy"


namespace fs = boost::filesystem;
using namespace boost::posix_time;
using namespace boost::gregorian;

namespace {

std::pair<std::string, std::string> parse_for_isystem(const std::string& s)
{
  if (s.find("-isystem") == 0) {
    return std::make_pair("isystem", std::string());
  }
  else if (s.find("-isysroot") == 0) {
    return std::make_pair("isysroot", std::string());
  }
  else {
    return std::make_pair(std::string(), std::string());
  }
}


eyestep::Grove scan_sources(const std::vector<eyestep::Source>& sources,
                            const std::vector<std::string>& incl_paths,
                            const std::vector<std::string>& defs)
{
  eyestep::Grove grove;
  eyestep::Node* root = grove.set_root_node(eyestep::root_class_definition());

  root->set_property("start-time",
                     to_iso_extended_string(microsec_clock::local_time()));

  if (!sources.empty()) {
    for (const auto& src : sources) {
      std::cout << "Scan " << src._srcfile << " ...";
      std::cout.flush();

      std::unique_ptr<eyestep::IScanner> scanner =
        eyestep::make_scanner_for_file(src._srcfile);

      if (scanner) {
        eyestep::Node* nd =
          scanner->scan_file(grove, src._srcfile,
                             eyestep::utils::join_list(incl_paths,
                                                       src._locincls),
                             eyestep::utils::join_list(defs, src._locdefs));
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

std::unique_ptr<eyestep::IProcessor> find_processor(const std::string& backend)
{
  if (backend == "debug") {
    return estd::make_unique<eyestep::DebugProcessor>();
  }
  else if (backend == "html") {
    return estd::make_unique<eyestep::HtmlProcessor>();
  }

  return nullptr;
}

fs::path deduce_output_file(const std::string& outf,
                            const std::vector<eyestep::Source>& sources,
                            const std::string& default_ext)
{
  if (!outf.empty()) {
    return outf;
  }

  if (!sources.empty()) {
    auto first = sources[0]._srcfile;
    return first.replace_extension(default_ext).filename();
  }

  return fs::path();
}

} // anon namespace


int main(int argc, char** argv)
{
  bool verbose = false;

  try {
    namespace po = boost::program_options;

    // Declare the supported options.
    po::options_description desc("Allowed options");

    // clang-format off
    desc.add_options()
      ("help,h",         "produce help message")
      ("verbose,v",      "being verbose")
      ("file,f",         po::value<std::string>(),
                         "read files to scan from arg")
      ("output,o",       po::value<std::string>(),
                         "write result to arg")
      ("backend,b",      po::value<std::string>(),
                         "use backend processor")
      ("template,t",     po::value<std::string>(),
                         "use template")
      ("include-path,I", po::value<std::vector<std::string>>()->composing(),
                         "add include path to C parser")
      ("defs,D",         po::value<std::vector<std::string>>()->composing(),
                         "add preprocessor defines")
      ("isysroot",       po::value<std::vector<std::string>>()->composing(),
                         "ignored")
      ("isystem",        po::value<std::vector<std::string>>()->composing(),
                         "-isystem arg, like -I")
      ("input-file",     po::value<std::vector<std::string>>(),
                         "input file")
      ;

    po::options_description hidden("Hidden options");
    hidden.add_options()
      ("sairy-prefix",   po::value<std::string>()->default_value(SAIRY_DEFAULT_PREFIX), "")
      ;
    // clang-format on

    po::options_description options;
    options.add(desc).add(hidden);

    po::positional_options_description p;
    p.add("input-file", -1);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv)
                .options(options)
                .positional(p)
                .extra_parser(&parse_for_isystem)
                .run(),
              vm);
    po::store(po::parse_environment(options, [](const std::string& opt) {
      if (opt == "SAIRY_PREFIX")
        return std::string("sairy-prefix");
      return std::string();
    }), vm);
    po::notify(vm);

    if (vm.count("help")) {
      std::cout << desc << "\n";
      exit(1);
    }
    if (vm.count("verbose")) {
      verbose = true;
    }


    std::vector<std::string> incl_paths;
    if (vm.count("include-path")) {
      incl_paths = vm["include-path"].as<std::vector<std::string>>();
    }

    if (vm.count("isystem")) {
      std::vector<std::string> v = vm["isystem"].as<std::vector<std::string>>();
      incl_paths.insert(incl_paths.end(), v.begin(), v.end());
    }

    std::vector<std::string> defs;
    if (vm.count("defs")) {
      defs = vm["defs"].as<std::vector<std::string>>();
    }

    std::string outf;
    if (vm.count("output")) {
      outf = vm["output"].as<std::string>();
    }

    std::string templ_path;
    if (vm.count("template")) {
      templ_path = vm["template"].as<std::string>();
    }

    std::string prefix_path;
    if (vm.count("sairy-prefix")) {
      prefix_path = vm["sairy-prefix"].as<std::string>();
    }

    std::string backend;
    if (vm.count("backend")) {
      backend = vm["backend"].as<std::string>();
    }

    if (verbose) {
      std::cout << "incl paths  : " << eyestep::utils::join(incl_paths, " ")
                << std::endl;
      std::cout << "defs        : " << eyestep::utils::join(defs, " ")
                << std::endl;
      std::cout << "outf        : " << outf << std::endl;
      std::cout << "prefix path : " << prefix_path << std::endl;
      std::cout << "templ_path  : " << templ_path << std::endl;
    }

    std::vector<eyestep::Source> sources;

    if (vm.count("file") > 0) {
      sources = eyestep::read_scan_db(vm["file"].as<std::string>());
    }
    else if (vm.count("input-file") > 0) {
      std::vector<std::string> inputs =
        vm["input-file"].as<std::vector<std::string>>();
      for (const auto& input : inputs) {
        sources.emplace_back(input, std::vector<std::string>(),
                             std::vector<std::string>());
      }
    }

    eyestep::Grove grove = scan_sources(sources, incl_paths, defs);
    if (verbose) {
      eyestep::serialize(std::cout, grove.root_node());
    }

    if (!templ_path.empty()) {
      auto processor = find_processor(backend);
      if (processor) {
        processor->set_output_file(
          deduce_output_file(outf, sources,
                             processor->default_output_extension()));

        eyestep::StyleEngine engine(eyestep::utils::split_paths(prefix_path),
                                    processor->proc_id());
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
