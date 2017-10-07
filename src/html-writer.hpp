// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fspp/filesystem.hpp"
#include "fspp/utils.hpp"

#include <functional>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>


namespace eyestep {
namespace html {

  namespace detail {
    struct StyleCtx;
  }


  struct Attr
  {
    std::string _key;
    std::string _value;
  };
  using Attrs = std::vector<Attr>;


  struct Doctype
  {
    std::string _publicid;
    std::string _systemid;
  };


  extern const Doctype k_XHTML_1_0_TRANSITIONAL_DTD;
  extern const Doctype k_XHTML_1_1_DTD;


  class Writer
  {
    filesystem::path _path;
    filesystem::File _file;
    Doctype _doctype;
    std::string _generator;
    const detail::StyleCtx* _ctx;
    bool _has_header = false;

  public:
    Writer(const Doctype& doctype, const std::string& generator,
           const detail::StyleCtx& ctx);

    bool is_open() const {
      return _file.is_valid() && _file.is_open();
    }

    void open(const filesystem::path& path);

    void write_attrs(const Attrs& attrs);

    void entity(const std::string& name);
    void open_tag(const std::string& tag, const Attrs& attrs = {});
    void close_tag(const std::string& tag);
    void empty_tag(const std::string& tag, const Attrs& attrs = {});
    void text_tag(const std::string& tag, const std::string& value,
                  const Attrs& attrs = {});
    void write_text(const std::string& value);

    void write_style(const std::string& text);
    void write_link(const std::string& rel, const Attrs& attrs = {});

    void doctype();
    void
    header(const std::string& title, const std::string& author, const std::string& desc,
           const std::function<void(std::ostream&)>& style_proc = [](std::ostream&) {});

    bool has_header() const {
      return _has_header;
    }

    void footer();

    void newln();
  };


  class CSSWriter
  {
    filesystem::path _path;
    filesystem::File _file;
    std::string _generator;
    std::unordered_map<std::string, std::string> _props_cache;

  public:
    CSSWriter(const std::string& generator);

    void open(const filesystem::path& path);

    bool is_open() const {
      return _file.is_valid() && _file.is_open();
    }

    void write_rule(const std::string& selector, const std::string& props);

    std::string add_rule(const std::string& tag, const std::string& props);
  };


  class Tag
  {
    Writer* _writer = nullptr;
    std::string _tag;

  public:
    Tag() = default;

    Tag(Writer& writer, const std::string& tag, const Attrs& attrs = {})
      : _writer(&writer)
      , _tag(tag) {
      _writer->open_tag(tag, attrs);
    }

    Tag(Tag&& other)
      : _writer(std::move(other._writer))
      , _tag(std::move(other._tag)) {
      other._writer = nullptr;
    }

    Tag& operator=(Tag&& other) {
      _writer = std::move(other._writer);
      _tag = std::move(other._tag);
      other._writer = nullptr;
      return *this;
    }

    ~Tag() {
      if (_writer) {
        _writer->close_tag(_tag);
      }
    }
  };
} // ns html
} // ns eyestep
