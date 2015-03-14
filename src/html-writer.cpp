// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "html-writer.hpp"

#include <ostream>
#include <iostream>
#include <sstream>


namespace eyestep {
namespace html {

  namespace {
    void escape_str_to_stream(std::ostream& os, const std::string& str)
    {
      for (const auto c : str) {
        switch (c) {
        case '<':
          os << "&lt;";
          break;
        case '>':
          os << "&gt;";
          break;
        case '&':
          os << "&amp;";
          break;
        default:
          os << c;
        }
      }
    }


    void escape_attr_str_to_stream(std::ostream& os, const std::string& str)
    {
      for (const auto c : str) {
        switch (c) {
        case '<':
          os << "&lt;";
          break;
        case '>':
          os << "&gt;";
          break;
        case '&':
          os << "&amp;";
          break;
        case '"':
          os << "&quot;";
          break;
        default:
          os << c;
        }
      }
    }


    std::string escape_attr_str(const std::string& str)
    {
      std::stringstream ss;
      escape_attr_str_to_stream(ss, str);
      return ss.str();
    }
  } // anon ns


  const Doctype kXHTML_1_0_TRANSITIONAL_DTD =
      {"-//W3C//DTD XHTML 1.0 Transitional//EN",
       "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"};
  const Doctype kXHTML_1_1_DTD =
      {"-//W3C//DTD XHTML 1.1//EN",
       "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"};


  Writer::Writer(const Doctype& doctype, const std::string& generator)
      : mDoctype(doctype), mGenerator(generator)
  {
  }


  void Writer::open(const boost::filesystem::path& path)
  {
    mPath = path;
    mStream.open(path.string(), std::ios_base::out | std::ios_base::binary |
                                    std::ios_base::trunc);

    if (mStream.fail()) {
      std::cerr << "Opening file '" << mPath << "' failed" << std::endl;
    }
  }


  void Writer::write_attrs(const Attrs& attrs)
  {
    for (const auto& attr : attrs) {
      mStream << " " << attr.mKey << "='";
      escape_attr_str_to_stream(mStream, attr.mValue);
      mStream << "'";
    }
  }

  void Writer::entity(const std::string& name)
  {
    mStream << "&" << name << ";";
  }


  void Writer::open_tag(const std::string& tag, const Attrs& attrs)
  {
    mStream << "<" << tag;
    write_attrs(attrs);
    mStream << ">";
  }


  void Writer::close_tag(const std::string& tag)
  {
    mStream << "</" << tag << ">";
  }


  void Writer::empty_tag(const std::string& tag, const Attrs& attrs)
  {
    mStream << "<" << tag;
    write_attrs(attrs);
    mStream << "/>";
  }


  void Writer::text_tag(const std::string& tag, const std::string& value,
                        const Attrs& attrs)
  {
    open_tag(tag, attrs);
    escape_str_to_stream(mStream, value);
    close_tag(tag);
  }


  void Writer::write_text(const std::string& value)
  {
    escape_str_to_stream(mStream, value);
  }


  void Writer::write_style(const std::string& text)
  {
    open_tag("style", {{"type", "text/css"}});
    mStream << text;
    close_tag("style");
  }


  void Writer::doctype()
  {
    mStream << "<!DOCTYPE html PUBLIC '" << mDoctype.mPublicId << "' '"
            << mDoctype.mSystemId << "'>" << std::endl;
  }


  void Writer::header(const std::string& title, const std::string& author,
                      const std::string& desc,
                      const std::function<void(std::ostream&)>& style_proc)
  {
    doctype();
    open_tag("html", {Attr{"xmlns", "http://www.w3.org/1999/xhtml"}});

    open_tag("head");
    text_tag("title", title);
    empty_tag("meta", {Attr{"http-equiv", "Content-Type"},
                       Attr{"content", "text/html; charset=UTF-8"}});
    if (!author.empty()) {
      empty_tag("meta", {Attr{"name", "author"},
                         Attr{"content", escape_attr_str(author)}});
    }
    if (!desc.empty()) {
      empty_tag("meta", {Attr{"name", "description"},
                         Attr{"content", escape_attr_str(desc)}});
    }

    empty_tag("meta", {Attr{"name", "generator"}, Attr{"content", mGenerator}});

    style_proc(mStream);

    close_tag("head");
    open_tag("body");
  }


  void Writer::footer()
  {
    close_tag("body");
    close_tag("html");
  }


} // ns html
} // ns eyestep
