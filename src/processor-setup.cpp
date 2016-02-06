// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "processor-setup.hpp"
#include "processor.hpp"
#include "utils.hpp"

#include "debug-processor.hpp"
#include "html-processor.hpp"
#include "tex-processor.hpp"
#include "pdf-processor.hpp"

#include <map>
#include <string>
#include <cassert>
#include <iostream>


namespace eyestep {

namespace po = boost::program_options;

namespace {
  using ProcessorFactoryFunc =
    std::function<std::unique_ptr<IProcessor>(const po::variables_map& args)>;
  using ProcessorFactoryMap = std::map<std::string, ProcessorFactoryFunc>;

  ProcessorFactoryMap s_processor_factory_map;

  template <typename T>
  void register_processor_factory()
  {
    T processor;
    const auto id = processor.proc_id();

    const auto i_find = s_processor_factory_map.find(id);
    assert(i_find == s_processor_factory_map.end());
    s_processor_factory_map[id] =
      [](const po::variables_map& args) { return estd::make_unique<T>(args); };
  }


  ProcessorFactoryMap& processor_registry()
  {
    if (s_processor_factory_map.empty()) {
      register_processor_factory<DebugProcessor>();
      register_processor_factory<HtmlProcessor>();
      register_processor_factory<TexProcessor>();
      register_processor_factory<PdfProcessor>();
    }

    return s_processor_factory_map;
  }
} // anon ns


po::options_description processor_options()
{
  std::vector<std::string> procs;
  std::vector<po::options_description> options;

  for (const auto& reg : processor_registry()) {
    auto processor = reg.second(po::variables_map{});
    if (processor) {
      procs.push_back(processor->proc_id());
      options.push_back(processor->program_options());
    }
  }

  std::string title = std::string("RENDERERS [") + utils::join(procs, ", ") + "]";
  po::options_description result(title);

  for (const auto& opt : options) {
    result.add(opt);
  }

  return result;
}

std::unique_ptr<eyestep::IProcessor>
make_processor_for_file(const std::string& proc_id,
                        const po::variables_map& args)
{
  const auto& registry = processor_registry();

  const auto i_processor_factory = registry.find(proc_id);
  if (i_processor_factory != registry.end()) {
    return i_processor_factory->second(args);
  }

  return nullptr;
}

} // ns eyestep
