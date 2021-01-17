// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "debug-processor.hpp"
#include "fo-processor.hpp"
#include "fo.hpp"
#include "sosofo.hpp"

#include "fspp/filesystem.hpp"

#include "cxxopts.hpp"

#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>

namespace eyestep {

namespace fs = filesystem;

namespace {
  class DebugFoProcessor : public IFoProcessor<DebugProcessor>
  {
  public:
    void render(DebugProcessor* processor, const IFormattingObject* fo) const override {
      struct DebugPropertySpecVisitor
      {
        void operator()(const fo::None&) {
          std::cout << "<unspecified>";
        }
        void operator()(const fo::LengthSpec& ls) {
          std::cout << ls;
        }
        void operator()(bool val) {
          std::cout << (val ? "yes" : "no");
        }
        void operator()(int val) {
          std::cout << val;
        }
        void operator()(const std::string& val) {
          std::cout << val;
        }
        void operator()(const fo::Color& co) {
          std::cout << co;
        }

        void operator()(const std::shared_ptr<Sosofo>& val) {
          std::cout << "<sosofo>";
        }

        void operator()(const std::shared_ptr<fo::ICompoundValue>& val) {
          std::cout << "<compound:" << val->type_id() << ">";
        }

        void operator()(const fo::Address& adr) {
          std::cout << adr;
        }

        void operator()(const std::shared_ptr<fo::IExpr>& val) {
          std::stringstream ss;
          val->write_to_stream(ss);
          std::cout << "<expr:" << ss.str() << ">";

          std::cout << " -> [";
          auto expr_val = val->eval();
          fo::apply(DebugPropertySpecVisitor(), expr_val);
          std::cout << "]";
        }
      };

      for (const auto& spec : fo->properties()) {
        std::cout << "      {" << spec._name << ": ";

        fo::apply(DebugPropertySpecVisitor(), spec._value);

        std::cout << " }" << std::endl;
      }

#if 0
      for (const auto& spec : fo->default_properties()) {
        auto prop = processor->property(fo, spec._name);
        if (prop) {
          std::cout << "      {" << spec._name << ": ";
          fo::apply(DebugPropertySpecVisitor(), spec._value);
          std::cout << " } -> ";

          std::cout << " [" << spec._name << ": ";
          fo::apply(DebugPropertySpecVisitor(), prop->_value);
          std::cout << " ]" << std::endl;
        }
      }
#endif

      for (const auto& portnm : fo->ports()) {
        std::cout << "DEBUG: PORT -> " << portnm << std::endl;
        processor->render_sosofo(&fo->port(portnm));
      }
    }
  };

} // namespace

DebugProcessor::DebugProcessor(const cxxopts::ParseResult& /*args*/) {}


void DebugProcessor::add_program_options(cxxopts::Options& options) const {
  options.add_options("Debug renderer");
}


const IFoProcessor<DebugProcessor>*
DebugProcessor::lookup_fo_processor(const std::string& fo_classname) const {
  static auto procs =
    std::map<std::string, std::shared_ptr<IFoProcessor<DebugProcessor>>>{
      {"#literal", std::make_shared<DebugFoProcessor>()},
      {"#paragraph", std::make_shared<DebugFoProcessor>()},
      {"#paragraph-break", std::make_shared<DebugFoProcessor>()},
      {"#display-group", std::make_shared<DebugFoProcessor>()},
      {"#sequence", std::make_shared<DebugFoProcessor>()},
      {"#line-field", std::make_shared<DebugFoProcessor>()},
      {"#anchor", std::make_shared<DebugFoProcessor>()},
      {"#page-number", std::make_shared<DebugFoProcessor>()},
      {"#simple-page-sequence", std::make_shared<DebugFoProcessor>()},
      {"#box", std::make_shared<DebugFoProcessor>()},
      {"#screen-set", std::make_shared<DebugFoProcessor>()},
      {"#simple-column-set-sequence", std::make_shared<DebugFoProcessor>()},
      {"#external-graphic", std::make_shared<DebugFoProcessor>()},
    };

  auto i_find = procs.find(fo_classname);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void DebugProcessor::before_rendering() {
  std::cout << "DEBUG: Processor: " << proc_id() << std::endl;
}


void DebugProcessor::render_sosofo(const Sosofo* sosofo) {
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


void DebugProcessor::render_fo(const IFormattingObject* fo) {
  std::cout << "  DEBUG: fo: " << fo->classname() << std::endl;

  Super::render_fo(fo);
}

} // namespace eyestep
