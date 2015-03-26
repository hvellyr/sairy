// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "scanner-setup.hpp"
#include "scanner.hpp"

#include "cpp-scanner.hpp"

#include <boost/filesystem.hpp>

#include <unordered_map>
#include <string>


namespace eyestep {

namespace fs = boost::filesystem;


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
    else if (i_parser_type->second == eyestep::ScribeScanner::kId) {
      return estd::make_unique<eyestep::ScribeScanner>();
    }
  }

  return nullptr;
}



} // ns eyestep
