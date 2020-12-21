// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "scanner-setup.hpp"
#include "estd/memory.hpp"
#include "scanner.hpp"
#include "tool-setup.hpp"
#include "utils.hpp"

#include "cxxopts.hpp"
#include "lexicon-scanner.hpp"
#include "textbook-scanner.hpp"

#include "fspp/filesystem.hpp"

#include <cassert>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>


namespace eyestep {

namespace fs = filesystem;


namespace {
  using ScannerFactoryFunc =
    std::function<std::unique_ptr<IScanner>(const ToolSetup& setup,
                                            const cxxopts::ParseResult& args)>;
  using ScannerClassFactoryMap = std::map<std::string, ScannerFactoryFunc>;

  using ScannerExtensionMap = std::unordered_map<std::string, std::string>;

  auto s_scanner_factory_map = ScannerClassFactoryMap{};
  auto s_scanner_extension_map = ScannerExtensionMap{};


  template <typename ScannerClass>
  void register_scanner_factory() {
    auto scanner = ScannerClass{};
    const auto id = scanner.scanner_id();

    assert(s_scanner_factory_map.find(id) == s_scanner_factory_map.end());
    s_scanner_factory_map[id] = [](const ToolSetup& setup,
                                   const cxxopts::ParseResult& args) {
      return estd::make_unique<ScannerClass>(setup, args);
    };

    for (const auto& ext : scanner.supported_extensions()) {
      assert(s_scanner_extension_map.find(ext) == s_scanner_extension_map.end());
      s_scanner_extension_map[ext] = id;
    }
  }


  ScannerClassFactoryMap& scanner_registry() {
    if (s_scanner_factory_map.empty()) {
      register_scanner_factory<TextbookScanner>();
      register_scanner_factory<LexiconScanner>();
    }

    return s_scanner_factory_map;
  }
} // namespace


std::vector<std::string> all_scanners() {
  auto scanner_ids = std::vector<std::string>{};

  for (const auto& reg : scanner_registry()) {
    auto scanner = reg.second(ToolSetup{}, cxxopts::ParseResult{});
    if (scanner) {
      scanner_ids.push_back(scanner->scanner_id());
    }
  }

  return scanner_ids;
}


void add_scanner_options(cxxopts::Options& options) {
  for (const auto& reg : scanner_registry()) {
    auto scanner = reg.second(ToolSetup{}, cxxopts::ParseResult{});
    if (scanner) {
      scanner->add_program_options(options);
    }
  }
}


std::unique_ptr<eyestep::IScanner>
make_scanner_for_file(const fs::path& file, const ToolSetup& setup,
                      const cxxopts::ParseResult& args) {
  const auto& registry = scanner_registry();

  auto ext = file.extension().string();
  const auto i_scanner_type = s_scanner_extension_map.find(ext);

  if (i_scanner_type != s_scanner_extension_map.end()) {
    const auto i_scanner_factory = registry.find(i_scanner_type->second);
    if (i_scanner_factory != registry.end()) {
      return i_scanner_factory->second(setup, args);
    }
  }

  return nullptr;
}

} // namespace eyestep
