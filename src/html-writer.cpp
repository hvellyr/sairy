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


  const Doctype k_XHTML_1_0_TRANSITIONAL_DTD =
    {"-//W3C//DTD XHTML 1.0 Transitional//EN",
     "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"};
  const Doctype k_XHTML_1_1_DTD =
    {"-//W3C//DTD XHTML 1.1//EN",
     "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"};


  Writer::Writer(const Doctype& doctype, const std::string& generator)
    : _doctype(doctype), _generator(generator)
  {
  }


  void Writer::open(const boost::filesystem::path& path)
  {
    _path = path;
    _stream.open(path.string(), std::ios_base::out | std::ios_base::binary |
                                  std::ios_base::trunc);

    if (_stream.fail()) {
      std::cerr << "Opening file '" << _path << "' failed" << std::endl;
    }
  }


  void Writer::write_attrs(const Attrs& attrs)
  {
    if (_stream.is_open()) {
      for (const auto& attr : attrs) {
        _stream << " " << attr._key << "='";
        escape_attr_str_to_stream(_stream, attr._value);
        _stream << "'";
      }
    }
  }

  void Writer::entity(const std::string& name)
  {
    if (_stream.is_open()) {
      _stream << "&" << name << ";";
    }
  }


  void Writer::open_tag(const std::string& tag, const Attrs& attrs)
  {
    if (_stream.is_open()) {
      _stream << "<" << tag;
      write_attrs(attrs);
      _stream << ">";
    }
  }


  void Writer::close_tag(const std::string& tag)
  {
    if (_stream.is_open()) {
      _stream << "</" << tag << ">";
    }
  }


  void Writer::empty_tag(const std::string& tag, const Attrs& attrs)
  {
    if (_stream.is_open()) {
      _stream << "<" << tag;
      write_attrs(attrs);
      _stream << "/>";
    }
  }


  void Writer::text_tag(const std::string& tag, const std::string& value,
                        const Attrs& attrs)
  {
    if (_stream.is_open()) {
      open_tag(tag, attrs);
      escape_str_to_stream(_stream, value);
      close_tag(tag);
    }
  }


  void Writer::write_text(const std::string& value)
  {
    if (_stream.is_open()) {
      escape_str_to_stream(_stream, value);
    }
  }


  void Writer::write_style(const std::string& text)
  {
    if (_stream.is_open()) {
      open_tag("style", {{"type", "text/css"}});
      newln();
      _stream << text;
      close_tag("style");
      newln();
    }
  }


  void Writer::doctype()
  {
    if (_stream.is_open()) {
      _stream << "<!DOCTYPE html PUBLIC '" << _doctype._publicid << "' '"
              << _doctype._systemid << "'>";
      newln();
    }
  }


  void Writer::header(const std::string& title, const std::string& author,
                      const std::string& desc,
                      const std::function<void(std::ostream&)>& style_proc)
  {
    if (_stream.is_open()) {
      doctype();
      open_tag("html", {Attr{"xmlns", "http://www.w3.org/1999/xhtml"}});
      newln();

      open_tag("head");
      newln();
      text_tag("title", title);
      newln();
      empty_tag("meta", {Attr{"http-equiv", "Content-Type"},
                         Attr{"content", "text/html; charset=UTF-8"}});
      newln();
      if (!author.empty()) {
        empty_tag("meta", {Attr{"name", "author"},
                           Attr{"content", escape_attr_str(author)}});
        newln();
      }
      if (!desc.empty()) {
        empty_tag("meta", {Attr{"name", "description"},
                           Attr{"content", escape_attr_str(desc)}});
        newln();
      }

      empty_tag("meta",
                {Attr{"name", "generator"}, Attr{"content", _generator}});
      newln();

      style_proc(_stream);

      close_tag("head");
      newln();
      open_tag("body");
      newln();
    }
  }


  void Writer::footer()
  {
    if (_stream.is_open()) {
      close_tag("body");
      newln();
      close_tag("html");
      newln();
    }
  }

  void Writer::newln()
  {
    if (_stream.is_open()) {
      _stream << std::endl;
    }
  }

} // ns html
} // ns eyestep
