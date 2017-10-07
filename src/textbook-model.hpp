// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "nodes.hpp"

#include "fspp/estd/optional.hpp"

#include <list>
#include <memory>
#include <sstream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>


namespace eyestep {
namespace textbook {

  enum AttrType
  {
    k_attr_id,
    k_attr_str,
    k_attr_fref, //!< fileref path
  };


  class AttrSpec
  {
    std::string _name;
    AttrType _type;
    bool _is_optional;
    bool _is_data;

  public:
    AttrSpec(const std::string& name, AttrType type, bool is_opt, bool is_data)
      : _name(name)
      , _type(type)
      , _is_optional(is_opt)
      , _is_data(is_data) {}

    bool is_optional() const {
      return _is_optional;
    }
    bool is_data() const {
      return _is_data;
    }

    std::string name() const {
      return _name;
    }
    AttrType type() const {
      return _type;
    }
  };


  class TagSpec
  {
    std::string _tagnm;
    std::vector<AttrSpec> _attrspecs;
    std::tuple<size_t, size_t> _attrc;
    bool _is_env;
    bool _is_mixed;
    bool _starts_p;
    bool _is_block;

  public:
    TagSpec()
      : _is_env(false)
      , _is_mixed(false)
      , _starts_p(false)
      , _is_block(false) {}

    TagSpec(const std::string& tagnm, const std::vector<AttrSpec>& attrspecs,
            std::tuple<size_t, size_t> attrc, bool is_env = false, bool is_mixed = false,
            bool starts_p = false, bool is_block = false)
      : _tagnm(tagnm)
      , _attrspecs(attrspecs)
      , _attrc(attrc)
      , _is_env(is_env)
      , _is_mixed(is_mixed)
      , _starts_p(starts_p)
      , _is_block(is_block) {}

    bool is_env() const {
      return _is_env;
    }

    bool is_mixed() const {
      return _is_mixed;
    }

    bool starts_p() const {
      return _starts_p;
    }

    bool is_block() const {
      return _is_block;
    }

    std::string tag_name() const {
      return _tagnm;
    }

    size_t min_args() const {
      return std::get<0>(_attrc);
    }

    size_t max_args() const {
      return std::get<1>(_attrc);
    }

    std::vector<AttrSpec> attrspecs() const {
      return _attrspecs;
    }
  };


  class DocSpec
  {
    std::unordered_map<std::string, TagSpec> _model;

  public:
    void add(const TagSpec& tagspec) {
      _model[tagspec.tag_name()] = tagspec;
    }

    estd::optional<TagSpec> lookup(const std::string& tagnm) {
      auto i_find = _model.find(tagnm);
      if (i_find != _model.end())
        return i_find->second;

      return {};
    }
  };

} // ns textbook
} // ns eyestep
