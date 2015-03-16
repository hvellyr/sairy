// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include <ostream>
#include <string>
#include <vector>
#include <functional>


namespace eyestep {
namespace html {

  struct Attr {
    std::string mKey;
    std::string mValue;
  };
  using Attrs = std::vector<Attr>;

  struct Doctype {
    std::string mPublicId;
    std::string mSystemId;
  };

  extern const Doctype kXHTML_1_0_TRANSITIONAL_DTD;
  extern const Doctype kXHTML_1_1_DTD;

  class Writer {
    boost::filesystem::path mPath;
    boost::filesystem::ofstream mStream;
    Doctype mDoctype;
    std::string mGenerator;

  public:
    Writer(const Doctype& doctype, const std::string& generator);

    void open(const boost::filesystem::path& path);

    void write_attrs(const Attrs& attrs);

    void entity(const std::string& name);
    void open_tag(const std::string& tag, const Attrs& attrs = {});
    void close_tag(const std::string& tag);
    void empty_tag(const std::string& tag, const Attrs& attrs = {});
    void text_tag(const std::string& tag, const std::string& value,
                  const Attrs& attrs = {});
    void write_text(const std::string& value);

    void write_style(const std::string& text);

    void doctype();
    void header(const std::string& title, const std::string& author,
                const std::string& desc,
                const std::function<void(std::ostream&)>& style_proc =
                    [](std::ostream&) {});
    void footer();

    void newln();
  };


  class Tag {
    Writer& mWriter;
    const std::string& mTag;

  public:
    Tag(Writer& writer, const std::string& tag, const Attrs& attrs = {})
        : mWriter(writer), mTag(tag)
    {
      mWriter.open_tag(tag, attrs);
    }

    ~Tag() { mWriter.close_tag(mTag); }
  };
} // ns html
} // ns eyestep
