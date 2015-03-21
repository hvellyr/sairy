// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"

#include <memory>
#include <vector>


namespace eyestep {

class Sosofo {
public:
  /*! Creates an empty sosofo */
  Sosofo();
  Sosofo(const Sosofo& one, const Sosofo& two);
  Sosofo(const std::vector<Sosofo>& sosofos);

  /*! Creates a sosofo with exactly one formatting object */
  Sosofo(std::shared_ptr<IFormattingObject> FO);

  /*! Returns a new sosofo with all FOs from @p other appended */ 
  Sosofo concat(const Sosofo& other) const;

  bool empty() const;
  int length() const;

  const IFormattingObject* operator[](size_t idx) const;

private:
  std::vector<std::shared_ptr<IFormattingObject>> mFOs;
};

} // ns eyestep
