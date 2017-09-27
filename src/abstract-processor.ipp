// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "abstract-processor.hpp"
#include "fo-processor.hpp"
#include "sosofo.hpp"

#include "fspp/filesystem.hpp"

#include <boost/variant/get.hpp>

#include <iostream>
#include <string>


namespace eyestep {

namespace fs = filesystem;


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::set_output_file(const fs::path& output_file)
{
  _output_file = output_file;
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::render_processed_node(const Sosofo* sosofo)
{
  before_rendering();
  render_sosofo(sosofo);
  after_rendering();
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::before_rendering()
{
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::after_rendering()
{
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::render_sosofo(const Sosofo* sosofo)
{
  if (sosofo) {
    for (size_t i = 0; i < sosofo->length(); ++i) {
      render_fo((*sosofo)[i]);
    }
  }
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::render_fo(const IFormattingObject* fo)
{
  const IFoProcessor<ProcessorT>* foproc = lookup_fo_processor(fo->classname());
  if (!foproc) {
    std::cerr << "Flow object '" << fo->classname() << "' unhandled"
              << std::endl;
  }
  else {
    _props.push(fo->properties());

    foproc->render(static_cast<ProcessorT*>(this), fo);

    _props.pop();
  }
}


template <typename ProcessorT>
fo::PropertySpecOrNone
AbstractProcessor<ProcessorT>::property(const IFormattingObject* fo,
                                        const std::string& key) const
{
  return _props.get(key, fo->default_properties());
}


template <typename ProcessorT>
template <typename T>
boost::optional<T>
AbstractProcessor<ProcessorT>::property_or_none(const IFormattingObject* fo,
                                                const std::string& key) const
{
  auto prop = property(fo, key);
  if (prop) {
    if (const T* val = boost::get<const T>(&prop->_value)) {
      return *val;
    }
  }

  return boost::none;
}

template <typename ProcessorT>
template <typename T>
T AbstractProcessor<ProcessorT>::property(const IFormattingObject* fo,
                                          const std::string& key,
                                          T default_value) const
{
  auto prop = property(fo, key);
  if (prop) {
    if (const T* val = boost::get<const T>(&prop->_value)) {
      return *val;
    }
  }

  return default_value;
}

} // ns eyestep
