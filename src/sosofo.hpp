// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"

#include <iterator>
#include <memory>
#include <string>
#include <vector>


namespace eyestep {

class SosofoIterator;

class Sosofo
{
public:
  /*! Creates an empty sosofo */
  Sosofo();
  Sosofo(const Sosofo& one, const Sosofo& two);
  Sosofo(const std::vector<Sosofo>& sosofos);

  /*! Creates a sosofo with exactly one formatting object */
  Sosofo(std::shared_ptr<IFormattingObject> fo);
  Sosofo(const std::string& label, std::shared_ptr<IFormattingObject> fo);

  /*! Returns a new sosofo with all FOs from @p other appended */
  Sosofo concat(const Sosofo& other) const;

  void set_label(const std::string& lbl) {
    _label = lbl;
  }

  const std::string& label() const {
    return _label;
  }

  bool empty() const {
    return _fos.empty();
  }

  int length() const {
    return int(_fos.size());
  }

  const IFormattingObject* operator[](size_t idx) const {
    return _fos[idx].get();
  }

  SosofoIterator begin() const;
  SosofoIterator end() const;

private:
  std::string _label;
  std::vector<std::shared_ptr<IFormattingObject>> _fos;
};


class SosofoIterator {
  const Sosofo* _sosofo = nullptr;
  int _idx = 0;

public:
  using iterator_category = std::forward_iterator_tag;
  using value_type = const IFormattingObject;
  using difference_type = std::ptrdiff_t;
  using pointer = value_type*;
  using reference = value_type&;

  /*! the end iterator */
  SosofoIterator() = default;
  SosofoIterator(const Sosofo* sosofo, int idx) : _sosofo(sosofo), _idx(idx)
  {

    if (_idx < 0 || _idx >= _sosofo->length()) {
      *this = {};
    }
  }

  SosofoIterator& operator++()
  {
    ++_idx;
    if (_idx >= _sosofo->length()) {
      *this = {};
    }
    return *this;
  }

  SosofoIterator operator++(int)
  {
    SosofoIterator retval = *this;
    ++(*this);
    return retval;
  }

  bool operator==(SosofoIterator other) const
  {
    return _sosofo == other._sosofo && _idx == other._idx;
  }

  bool operator!=(SosofoIterator other) const { return !(*this == other); }

  reference operator*() const { return *(*_sosofo)[_idx]; }

  pointer operator->() const { return (*_sosofo)[_idx]; }
};

} // ns eyestep
