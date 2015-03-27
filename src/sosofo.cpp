// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "sosofo.hpp"

#include <algorithm>
#include <cassert>


namespace eyestep {

Sosofo::Sosofo()
{
}

Sosofo::Sosofo(const Sosofo& one, const Sosofo& two)
{
  _fos.insert(_fos.end(), one._fos.begin(), one._fos.end());
  _fos.insert(_fos.end(), two._fos.begin(), two._fos.end());
}

Sosofo::Sosofo(const std::vector<Sosofo>& sosofos)
{
  for (const auto& sosofo : sosofos) {
    _fos.insert(_fos.end(), sosofo._fos.begin(), sosofo._fos.end());
  }
}

Sosofo::Sosofo(std::shared_ptr<IFormattingObject> fo) : _fos({fo})
{
}

/*! Returns a new sosofo with all FOs from @p other appended */
Sosofo Sosofo::concat(const Sosofo& other) const
{
  Sosofo sosofo;
  sosofo._fos.insert(sosofo._fos.end(), _fos.begin(), _fos.end());
  sosofo._fos.insert(sosofo._fos.end(), other._fos.begin(), other._fos.end());
  return sosofo;
}

bool Sosofo::empty() const
{
  return _fos.empty();
}

int Sosofo::length() const
{
  return int(_fos.size());
}

const IFormattingObject* Sosofo::operator[](size_t idx) const
{
  return _fos[idx].get();
}

} // ns eyestep
