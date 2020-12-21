// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "processor-setup.hpp"
#include "estd/memory.hpp"
#include "processor.hpp"
#include "utils.hpp"

#include "debug-processor.hpp"
#include "html-processor.hpp"
#include "tex-processor.hpp"

#include "cxxopts.hpp"

#include <string>
#include <unordered_map>


namespace eyestep {

namespace {
  using ProcessorFactoryFunc =
    std::function<std::unique_ptr<IProcessor>(const cxxopts::ParseResult& args)>;
  using ProcessorFactoryMap = std::unordered_map<std::string, ProcessorFactoryFunc>;

  template <typename T>
  std::pair<std::string, ProcessorFactoryFunc> make_processor_factory() {
    const auto proc = T{};
    return std::make_pair(proc.proc_id(), [](const cxxopts::ParseResult& args) {
      return estd::make_unique<T>(args);
    });
  }

  ProcessorFactoryMap& processor_registry() {
    static auto s_processor_factory_map = ProcessorFactoryMap{
      make_processor_factory<DebugProcessor>(), //
      make_processor_factory<HtmlProcessor>(),  //
      make_processor_factory<TexProcessor>(),
    };

    return s_processor_factory_map;
  }
} // namespace


std::vector<std::string> all_processors() {
  auto proc_ids = std::vector<std::string>{};

  for (const auto& reg : processor_registry()) {
    auto processor = reg.second(cxxopts::ParseResult{});
    if (processor) {
      proc_ids.emplace_back(processor->proc_id());
    }
  }

  return proc_ids;
}


void add_processor_options(cxxopts::Options& options) {
  for (const auto& reg : processor_registry()) {
    auto processor = reg.second(cxxopts::ParseResult{});
    if (processor) {
      processor->add_program_options(options);
    }
  }
}


std::unique_ptr<eyestep::IProcessor> make_processor(const std::string& proc_id,
                                                    const cxxopts::ParseResult& args) {
  const auto& registry = processor_registry();

  const auto i_processor_factory = registry.find(proc_id);
  return i_processor_factory != registry.end() ? i_processor_factory->second(args)
                                               : nullptr;
}

} // namespace eyestep
