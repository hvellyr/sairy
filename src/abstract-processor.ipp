// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "abstract-processor.hpp"
#include "fo-processor.hpp"
#include "fo.hpp"
#include "sosofo.hpp"
#include "style-engine.hpp"

#include "fspp/filesystem.hpp"

#include <iostream>
#include <string>


namespace eyestep {

namespace fs = filesystem;

template <typename ProcessorT>
AbstractProcessor<ProcessorT>::AbstractProcessor()
  : _prop_lookup{&_props}
{
  _props.set_property_lookup(&_prop_lookup);
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::set_output_file(const fs::path& output_file) {
  _output_file = output_file;
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::render_processed_node(StyleEngine* engine,
                                                          const Sosofo* sosofo) {
  _engine = engine;
  try {
    before_rendering();
    render_sosofo(sosofo);
    after_rendering();
  }
  catch (...) {
    _engine = nullptr;
    throw;
  }

  _engine = nullptr;
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::before_rendering() {}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::after_rendering() {}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::render_sosofo(const Sosofo* sosofo) {
  if (sosofo) {
    for (const auto& fo : *sosofo) {
      render_fo(&fo);
    }
  }
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::render_fo(const IFormattingObject* fo) {
  auto foproc = lookup_fo_processor(fo->classname());
  if (!foproc) {
    std::cerr << "Flow object '" << fo->classname() << "' unhandled" << std::endl;
  }
  else {
    _props.push(fo);

    _engine->set_property_lookup(&_prop_lookup);
    foproc->render(static_cast<ProcessorT*>(this), fo);
    _engine->set_property_lookup(nullptr);

    _props.pop();
  }
}


template <typename ProcessorT>
template <typename T>
estd::optional<T>
AbstractProcessor<ProcessorT>::property_or_none(const std::string& key) const {
  return _props.lookup_or_none<T>(key);
}


template <typename ProcessorT>
template <typename T>
T AbstractProcessor<ProcessorT>::property(const std::string& key, T default_value) const {
  if (auto prop = _props.lookup_or_none<T>(key))
    return *prop;

  return default_value;
}


template <typename ProcessorT>
inline StyleEngine* AbstractProcessor<ProcessorT>::engine() {
  return _engine;
}

} // namespace eyestep
