// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

namespace eyestep {

class IFormattingObject;


template <typename ProcessorT>
class IFoProcessor
{
public:
  virtual ~IFoProcessor() {}

  virtual void render(ProcessorT* processor, const IFormattingObject* fo) const = 0;
};

} // ns eyestep
