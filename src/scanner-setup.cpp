// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "scanner-setup.hpp"
#include "scanner.hpp"

#include "cpp-scanner.hpp"
#include "textbook-scanner.hpp"

#include <boost/filesystem.hpp>

#include <unordered_map>
#include <string>
#include <cassert>
#include <iostream>


namespace eyestep {

namespace fs = boost::filesystem;
namespace po = boost::program_options;

namespace {
  using ScannerFactoryFunc =
    std::function<std::unique_ptr<IScanner>(const po::variables_map& args)>;
  using ScannerClassFactoryMap =
    std::unordered_map<std::string, ScannerFactoryFunc>;

  using ScannerExtensionMap = std::unordered_map<std::string, std::string>;

  ScannerClassFactoryMap s_scanner_factory_map;
  ScannerExtensionMap s_scanner_extension_map;

  template <typename ScannerClass>
  void register_scanner_factory()
  {
    ScannerClass scanner;
    const auto id = scanner.scanner_id();

    const auto i_find = s_scanner_factory_map.find(id);
    assert(i_find == s_scanner_factory_map.end());
    s_scanner_factory_map[id] = [](const po::variables_map& args) {
      return estd::make_unique<ScannerClass>(args);
    };

    for (const auto& ext : scanner.supported_extensions()) {
      const auto i_find = s_scanner_extension_map.find(ext);
      assert(i_find == s_scanner_extension_map.end());
      s_scanner_extension_map[ext] = id;
    }
  }


  ScannerClassFactoryMap& scanner_registry()
  {
    if (s_scanner_factory_map.empty()) {
      register_scanner_factory<CppScanner>();
      register_scanner_factory<TextbookScanner>();
    }

    return s_scanner_factory_map;
  }
} // anon ns


std::vector<po::options_description> scanner_options()
{
  std::vector<po::options_description> result;

  for (const auto& reg : scanner_registry()) {
    auto scanner = reg.second(po::variables_map{});
    if (scanner) {
      result.push_back(scanner->program_options());
    }
  }

  return result;
}

std::unique_ptr<eyestep::IScanner>
make_scanner_for_file(const fs::path& file, const po::variables_map& args)
{
  const auto& registry = scanner_registry();

  auto ext = file.extension().string();
  const auto i_scanner_type = s_scanner_extension_map.find(ext);

  if (i_scanner_type != s_scanner_extension_map.end()) {
    const auto i_scanner_factory = registry.find(i_scanner_type->second);
    if (i_scanner_factory != registry.end()) {
      return i_scanner_factory->second(args);
    }
  }

  return nullptr;
}

} // ns eyestep
