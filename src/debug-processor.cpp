// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "debug-processor.hpp"
#include "fo-processor.hpp"
#include "fo.hpp"
#include "sosofo.hpp"

#include <boost/filesystem.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>

#include <iostream>
#include <string>
#include <map>
#include <memory>


namespace eyestep {

namespace fs = boost::filesystem;
namespace po = boost::program_options;


namespace {
  class DebugPropertySpecVisitor : public boost::static_visitor<> {
  public:
    void operator()(const fo::Dimen& dimen) { std::cout << dimen; }
    void operator()(bool val) { std::cout << (val ? "yes" : "no"); }
    void operator()(int val) { std::cout << val; }
    void operator()(const std::string& val) { std::cout << val; }
    void operator()(const std::shared_ptr<Sosofo>& val)
    {
      std::cout << "<sosofo>";
    }
  };

  class DebugFoProcessor : public IFoProcessor<DebugProcessor> {
  public:
    void render(DebugProcessor* processor,
                const IFormattingObject* fo) const override
    {
      for (const auto& spec : fo->properties()) {
        std::cout << "      {" << spec._name << ": ";

        DebugPropertySpecVisitor visitor;
        boost::apply_visitor(visitor, spec._value);

        std::cout << " }" << std::endl;
      }

#if 0
      for (const auto& spec : fo->default_properties()) {
        DebugPropertySpecVisitor visitor;

        auto prop = processor->property(fo, spec._name);
        if (prop) {
          std::cout << "      {" << spec._name << ": ";
          boost::apply_visitor(visitor, spec._value);
          std::cout << " } -> ";

          std::cout << " [" << spec._name << ": ";
          boost::apply_visitor(visitor, prop->_value);
          std::cout << " ]" << std::endl;
        }
      }
#endif

      for (const auto& portnm : fo->ports()) {
        std::cout << "DEBUG: PORT -> " << portnm << std::endl;
        const Sosofo& port = fo->port(portnm);
        processor->render_sosofo(&port);
      }
    }
  };

} // ns anon


DebugProcessor::DebugProcessor()
{
}

DebugProcessor::DebugProcessor(const po::variables_map& /*args*/)
{
}

std::string DebugProcessor::proc_id() const
{
  return "debug";
}

std::string DebugProcessor::default_output_extension() const
{
  return std::string();
}

po::options_description DebugProcessor::program_options() const
{
  std::string opts_title =
    std::string("Debug renderer [selector: '") + proc_id() + "']";
  po::options_description desc(opts_title);

  return desc;
}

const IFoProcessor<DebugProcessor>*
DebugProcessor::lookup_fo_processor(const std::string& fo_classname) const
{
  static auto procs =
    std::map<std::string, std::shared_ptr<IFoProcessor<DebugProcessor>>>{
      {"#literal", std::make_shared<DebugFoProcessor>()},
      {"#paragraph", std::make_shared<DebugFoProcessor>()},
      {"#paragraph-break", std::make_shared<DebugFoProcessor>()},
      {"#display-group", std::make_shared<DebugFoProcessor>()},
      {"#simple-page-sequence", std::make_shared<DebugFoProcessor>()},
    };

  auto i_find = procs.find(fo_classname);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void DebugProcessor::before_rendering()
{
  std::cout << "DEBUG: Processor: " << proc_id() << std::endl;
}


void DebugProcessor::render_sosofo(const Sosofo* sosofo)
{
  if (!sosofo) {
    std::cout << "  DEBUG: sosofo: (null)" << std::endl;
  }
  else if (sosofo->empty()) {
    std::cout << "  DEBUG: sosofo: (empty)" << std::endl;
  }
  else {
    std::cout << "  DEBUG: sosofo: [" << sosofo->length() << "]" << std::endl;
  }

  Super::render_sosofo(sosofo);
}


void DebugProcessor::render_fo(const IFormattingObject* fo)
{
  std::cout << "  DEBUG: fo: " << fo->classname() << std::endl;

  Super::render_fo(fo);
}

} // ns eyestep
