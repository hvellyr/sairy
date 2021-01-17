// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "abstract-processor.hpp"
#include "fo-processor.hpp"
#include "sosofo.hpp"

#include "fspp/filesystem.hpp"

#include <iostream>
#include <string>


namespace eyestep {

namespace fs = filesystem;


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::set_output_file(const fs::path& output_file) {
  _output_file = output_file;
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::render_processed_node(const Sosofo* sosofo) {
  before_rendering();
  render_sosofo(sosofo);
  after_rendering();
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

    foproc->render(static_cast<ProcessorT*>(this), fo);

    _props.pop();
  }
}


template <typename ProcessorT>
template <typename T>
estd::optional<T>
AbstractProcessor<ProcessorT>::property_or_none(const std::string& key) const {
  return _props.lookup<T>(key);
}


template <typename ProcessorT>
template <typename T>
T AbstractProcessor<ProcessorT>::property(const std::string& key, T default_value) const {
  if (auto prop = _props.lookup<T>(key))
    return *prop;

  return default_value;
}

} // namespace eyestep
