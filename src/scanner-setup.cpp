// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "scanner-setup.hpp"
#include "scanner.hpp"
#include "utils.hpp"

#include "program_options/program_options.hpp"
#include "textbook-scanner.hpp"

#include "fspp/filesystem.hpp"

#include <cassert>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>


namespace eyestep {

namespace fs = filesystem;
namespace po = program_options;

namespace {
  using ScannerFactoryFunc =
    std::function<std::unique_ptr<IScanner>(const po::variables_map& args)>;
  using ScannerClassFactoryMap =
    std::map<std::string, ScannerFactoryFunc>;

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
      return ::estd::make_unique<ScannerClass>(args);
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
      register_scanner_factory<TextbookScanner>();
    }

    return s_scanner_factory_map;
  }
} // anon ns


po::options_description scanner_options()
{
  std::vector<std::string> parsers;
  std::vector<po::options_description> options;

  for (const auto& reg : scanner_registry()) {
    auto scanner = reg.second(po::variables_map{});
    if (scanner) {
      parsers.push_back(scanner->scanner_id());
      auto opts = scanner->program_options();
      if (!opts.empty()) {
        options.push_back(opts);
      }
    }
  }

  std::string title = std::string("PARSERS [") + utils::join(parsers, ", ") + "]";
  po::options_description result(title);

  for (const auto& opt : options) {
    result.add(opt);
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
