// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "processor-setup.hpp"
#include "estd/memory.hpp"
#include "processor.hpp"
#include "utils.hpp"

#include "debug-processor.hpp"
#include "html-processor.hpp"
#include "tex-processor.hpp"

#include "program_options/program_options.hpp"

#include <cassert>
#include <iostream>
#include <map>
#include <string>


namespace eyestep {

namespace po = program_options;

namespace {
  using ProcessorFactoryFunc =
    std::function<std::unique_ptr<IProcessor>(const po::variables_map& args)>;
  using ProcessorFactoryMap = std::map<std::string, ProcessorFactoryFunc>;

  ProcessorFactoryMap s_processor_factory_map;

  template <typename T>
  void register_processor_factory() {
    T processor;
    const auto id = processor.proc_id();

    assert(s_processor_factory_map.find(id) == s_processor_factory_map.end());
    s_processor_factory_map[id] = [](const po::variables_map& args) {
      return ::estd::make_unique<T>(args);
    };
  }


  ProcessorFactoryMap& processor_registry() {
    if (s_processor_factory_map.empty()) {
      register_processor_factory<DebugProcessor>();
      register_processor_factory<HtmlProcessor>();
      register_processor_factory<TexProcessor>();
    }

    return s_processor_factory_map;
  }
} // anon ns


po::options_description processor_options() {
  std::vector<std::string> procs;
  std::vector<po::options_description> options;

  for (const auto& reg : processor_registry()) {
    auto processor = reg.second(po::variables_map{});
    if (processor) {
      procs.emplace_back(processor->proc_id());

      auto opts = processor->program_options();
      if (!opts.empty()) {
        options.emplace_back(opts);
      }
    }
  }

  auto title = std::string("RENDERERS [") + utils::join(procs, ", ") + "]";
  auto result = po::options_description(title);

  for (const auto& opt : options) {
    result.add(opt);
  }

  return result;
}


std::unique_ptr<eyestep::IProcessor>
make_processor_for_file(const std::string& proc_id, const po::variables_map& args) {
  const auto& registry = processor_registry();

  const auto i_processor_factory = registry.find(proc_id);
  return i_processor_factory != registry.end()
    ? i_processor_factory->second(args)
    : nullptr;
}

} // ns eyestep
