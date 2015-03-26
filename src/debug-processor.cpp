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
        std::cout << "      {" << spec.mName << ": ";

        DebugPropertySpecVisitor visitor;
        boost::apply_visitor(visitor, spec.mValue);

        std::cout << " }" << std::endl;
      }

#if 0
      for (const auto& spec : fo->defaultProperties()) {
        DebugPropertySpecVisitor visitor;

        auto prop = processor->property(fo, spec.mName);
        if (prop) {
          std::cout << "      {" << spec.mName << ": ";
          boost::apply_visitor(visitor, spec.mValue);
          std::cout << " } -> ";

          std::cout << " [" << spec.mName << ": ";
          boost::apply_visitor(visitor, prop->mValue);
          std::cout << " ]" << std::endl;
        }
      }
#endif

      for (const auto& portnm : fo->ports()) {
        std::cout << "DEBUG: PORT -> " << portnm << std::endl;
        const Sosofo& port = fo->port(portnm);
        processor->renderSosofo(&port);
      }
    }
  };

} // ns anon


std::string DebugProcessor::procId() const
{
  return "#debug-processor";
}

std::string DebugProcessor::default_output_extension() const
{
  return std::string();
}

const IFoProcessor<DebugProcessor>*
DebugProcessor::lookupFoProcessor(const std::string& foClassName) const
{
  static auto procs =
    std::map<std::string, std::shared_ptr<IFoProcessor<DebugProcessor>>>{
      {"#literal", std::make_shared<DebugFoProcessor>()},
      {"#paragraph", std::make_shared<DebugFoProcessor>()},
      {"#paragraph-break", std::make_shared<DebugFoProcessor>()},
      {"#display-group", std::make_shared<DebugFoProcessor>()},
      {"#simple-page-sequence", std::make_shared<DebugFoProcessor>()},
    };

  auto i_find = procs.find(foClassName);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void DebugProcessor::beforeRendering()
{
  std::cout << "DEBUG: Processor: " << procId() << std::endl;
}


void DebugProcessor::renderSosofo(const Sosofo* sosofo)
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

  Super::renderSosofo(sosofo);
}


void DebugProcessor::renderFo(const IFormattingObject* fo)
{
  std::cout << "  DEBUG: fo: " << fo->className() << std::endl;

  Super::renderFo(fo);
}

} // ns eyestep
