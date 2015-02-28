// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "nodes.hpp"
#include "scm-context.hpp"
#include "source.hpp"
#include "utils.hpp"

#include "cpp-scanner.hpp"

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <boost/program_options/parsers.hpp>
#include <boost/range/adaptor/transformed.hpp>

#include <string>
#include <iostream>
#include <unordered_map>
#include <memory>

#define SAIRY_DEFAULT_PREFIX "/usr/local/share/sairy"


namespace fs = boost::filesystem;
using namespace boost::posix_time;
using namespace boost::gregorian;

namespace {

std::pair<std::string, std::string> parseForISystem(const std::string& s)
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


std::unique_ptr<eyestep::IScanner> make_scanner_for_file(const fs::path& file)
{
  static std::unordered_map<std::string, std::string> parser_map = {
      {".c", eyestep::CppScanner::kId},
      {".cpp", eyestep::CppScanner::kId},
      {".cxx", eyestep::CppScanner::kId},
      {".h", eyestep::CppScanner::kId},
      {".hh", eyestep::CppScanner::kId},
      {".hpp", eyestep::CppScanner::kId},
      {".hxx", eyestep::CppScanner::kId},
      {".ipp", eyestep::CppScanner::kId},
      {".m", eyestep::CppScanner::kId},
      {".mm", eyestep::CppScanner::kId},
  };

  auto i_parser_type = parser_map.find(file.extension().string());
  if (i_parser_type != parser_map.end()) {
    if (i_parser_type->second == eyestep::CppScanner::kId) {
      return estd::make_unique<eyestep::CppScanner>();
    }
  }

  return nullptr;
}


std::unique_ptr<eyestep::ISchemeContext>
setup_scheme_context(const std::vector<fs::path>& prefix_paths)
{
  auto ctx = eyestep::createSchemeContext();

  auto paths = boost::copy_range<std::vector<fs::path>>(
      prefix_paths | boost::adaptors::transformed(
                         [](const fs::path& path) { return path / "lib"; }));

  ctx->initialize(paths);

  auto init_path = fs::path("sairy") / "init.scm";
  if (!ctx->loadModuleFile(init_path)) {
    std::cerr << "Could not read " << init_path.string() << std::endl;
    return nullptr;
  }

  return std::move(ctx);
}


void load_template(eyestep::ISchemeContext* ctx, const fs::path& templPath,
                   const eyestep::Node* root)
{
  ctx->setupTemplateFunctions(root);

  if (!ctx->loadScript(templPath)) {
    std::cerr << "Could not read " << templPath.string() << std::endl;
    return;
  }
}


eyestep::Grove scan_sources(const std::vector<eyestep::Source>& sources,
                            const std::vector<std::string>& incl_paths,
                            const std::vector<std::string>& defs)
{
  eyestep::Grove grove;
  eyestep::Node* root = grove.setRootNode("project");

  root->setProperty("start-time",
                    to_iso_extended_string(microsec_clock::local_time()));

  if (!sources.empty()) {
    for (const auto& src : sources) {
      std::cout << "Scan " << src.mSrcfile << " ...";
      std::cout.flush();

      std::unique_ptr<eyestep::IScanner> scanner =
          make_scanner_for_file(src.mSrcfile);

      if (scanner) {
        eyestep::Node* nd =
            scanner->scanFile(grove, src.mSrcfile,
                              eyestep::utils::joinList(incl_paths,
                                                       src.mLocIncls),
                              eyestep::utils::joinList(defs, src.mLocDefs));
        std::cout << " ok" << std::endl;

        root->addChildNode(nd);
      }
      else {
        std::cout << " no scanner for filetype" << std::endl;
      }
    }
  }

  root->setProperty("end-time",
                    to_iso_extended_string(microsec_clock::local_time()));

  return grove;
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
                  .extra_parser(parseForISystem)
                  .run(),
              vm);
    po::store(po::parse_environment(options, [](const std::string& opt) {
                if (opt == "SAIRY_PREFIX")
                  return std::string("sairy-prefix");
                return std::string();
              }),
              vm);
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
      sources = eyestep::readScanDb(vm["file"].as<std::string>());
    }
    else if (vm.count("input-file") > 0) {
      std::vector<std::string> inputs =
          vm["input-file"].as<std::vector<std::string>>();
      for (const auto& input : inputs) {
        sources.emplace_back(input, std::vector<std::string>(),
                             std::vector<std::string>());
      }
    }

    auto scheme_ctx = std::move(
        setup_scheme_context(eyestep::utils::split_paths(prefix_path)));

    eyestep::Grove grove = scan_sources(sources, incl_paths, defs);
    if (verbose) {
      eyestep::serialize(std::cout, grove.rootNode());
    }

    if (!templ_path.empty()) {
      load_template(scheme_ctx.get(), templ_path, grove.rootNode());
    }
  }
  catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
  }

  exit(0);
}
