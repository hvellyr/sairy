// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "abstract-processor.hpp"
#include "fo-processor.hpp"
#include "sosofo.hpp"

#include <boost/filesystem.hpp>

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
    // mProps.push(fo->properties());

    foproc->render(static_cast<ProcessorT*>(this), fo);

    // mProps.pop();
  }
}

} // ns eyestep
