// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "abstract-processor.hpp"
#include "fo-processor.hpp"
#include "sosofo.hpp"

#include <boost/filesystem.hpp>
#include <boost/variant/get.hpp>

#include <iostream>
#include <string>


namespace eyestep {

namespace fs = boost::filesystem;


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::setOutputFile(const fs::path& outputFile)
{
  mOutputFile = outputFile;
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::renderProcessedNode(const Sosofo* sosofo)
{
  beforeRendering();
  renderSosofo(sosofo);
  afterRendering();
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::beforeRendering()
{
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::afterRendering()
{
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::renderSosofo(const Sosofo* sosofo)
{
  if (sosofo) {
    for (size_t i = 0; i < sosofo->length(); ++i) {
      renderFo((*sosofo)[i]);
    }
  }
}


template <typename ProcessorT>
void AbstractProcessor<ProcessorT>::renderFo(const IFormattingObject* fo)
{
  const IFoProcessor<ProcessorT>* foproc = lookupFoProcessor(fo->className());
  if (!foproc) {
    std::cerr << "Flow object '" << fo->className() << "' unhandled"
              << std::endl;
  }
  else {
    mProps.push(fo->properties());

    foproc->render(static_cast<ProcessorT*>(this), fo);

    mProps.pop();
  }
}


template <typename ProcessorT>
fo::PropertySpecOrNone
AbstractProcessor<ProcessorT>::property(const IFormattingObject* fo,
                                        const std::string& key) const
{
  return mProps.get(key, fo->defaultProperties());
}


template <typename ProcessorT>
template <typename T>
boost::optional<T>
AbstractProcessor<ProcessorT>::propertyOrNone(const IFormattingObject* fo,
                                              const std::string& key) const
{
  auto prop = property(fo, key);
  if (prop) {
    if (const T* val = boost::get<const T>(&prop->mValue)) {
      return *val;
    }
  }

  return boost::none;
}

template <typename ProcessorT>
template <typename T>
T AbstractProcessor<ProcessorT>::property(const IFormattingObject* fo,
                                          const std::string& key,
                                          T defaultValue) const
{
  auto prop = property(fo, key);
  if (prop) {
    if (const T* val = boost::get<const T>(&prop->mValue)) {
      return *val;
    }
  }

  return defaultValue;
}

} // ns eyestep
