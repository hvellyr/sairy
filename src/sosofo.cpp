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
  mFOs.insert(mFOs.end(), one.mFOs.begin(), one.mFOs.end());
  mFOs.insert(mFOs.end(), two.mFOs.begin(), two.mFOs.end());
}

Sosofo::Sosofo(const std::vector<Sosofo>& sosofos)
{
  for (const auto& sosofo : sosofos) {
    mFOs.insert(mFOs.end(), sosofo.mFOs.begin(), sosofo.mFOs.end());
  }
}

Sosofo::Sosofo(std::shared_ptr<IFormattingObject> FO) : mFOs({FO})
{
}

/*! Returns a new sosofo with all FOs from @p other appended */
Sosofo Sosofo::concat(const Sosofo& other) const
{
  Sosofo sosofo;
  sosofo.mFOs.insert(sosofo.mFOs.end(), mFOs.begin(), mFOs.end());
  sosofo.mFOs.insert(sosofo.mFOs.end(), other.mFOs.begin(), other.mFOs.end());
  return sosofo;
}

bool Sosofo::empty() const
{
  return mFOs.empty();
}

int Sosofo::length() const
{
  return int(mFOs.size());
}

const IFormattingObject* Sosofo::operator[](size_t idx) const
{
  return mFOs[idx].get();
}

} // ns eyestep
