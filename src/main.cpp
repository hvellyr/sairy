// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "nodes.hpp"
#include "source.hpp"
#include "utils.hpp"
#include "estd/memory.hpp"

#include "cpp-scanner.hpp"

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#include <string>
#include <iostream>
#include <memory>


namespace fs = boost::filesystem;

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
  auto ext = file.extension().string();

  if (ext == ".hpp" || ext == ".cpp" || ext == ".ipp") {
    return estd::make_unique<eyestep::CppScanner>();
  }
}

  return nullptr;
}

} // anon namespace


int main(int argc, char** argv)
{
  try {
    namespace po = boost::program_options;

    // Declare the supported options.
    po::options_description desc("Allowed options");

    // clang-format off
    desc.add_options()
      ("help,h",         "produce help message")
      ("file,f",         po::value<std::string>(),
                         "read files to scan from arg")
      ("output,o",       po::value<std::string>(),
                         "write result to arg")
      ("include-path,I", po::value<std::vector<std::string>>()->composing(),
                         "add include path to C parser")
      ("defs,D",         po::value<std::vector<std::string>>()->composing(),
                         "add preprocessor defines")
      ("isysroot",       po::value<std::vector<std::string>>()->composing(),
                         "ignored")
      ("isystem",        po::value<std::vector<std::string>>()->composing(),
                         "-isystem arg, like -I")
      ("input-file",     po::value<std::vector<std::string>>(),
                         "input file");
    // clang-format on

    po::positional_options_description p;
    p.add("input-file", -1);

    po::variables_map vm;
    po::store(po::command_line_parser(argc, argv)
                  .options(desc)
                  .positional(p)
                  .extra_parser(parseForISystem)
                  .run(),
              vm);
    po::notify(vm);

    if (vm.count("help")) {
      std::cout << desc << "\n";
      exit(1);
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


    if (!sources.empty()) {
      for (const auto& src : sources) {
        std::cout << "Scan " << src.mSrcfile << " ...";
        std::cout.flush();

        std::unique_ptr<eyestep::IScanner> scanner =
            make_scanner_for_file(src.mSrcfile);
        if (scanner) {
          eyestep::Node nd =
              scanner->scanFile(src.mSrcfile,
                                eyestep::utils::joinList(incl_paths,
                                                         src.mLocIncls),
                                eyestep::utils::joinList(defs, src.mLocDefs));
          std::cout << " ok" << std::endl;

          std::cout << nd << std::endl;
        }
        else {
          std::cout << " no scanner for filetype" << std::endl;
        }
      }
    }
  }
  catch (const std::exception& e) {
    std::cerr << e.what() << std::endl;
  }

  exit(0);
}
