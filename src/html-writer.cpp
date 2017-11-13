// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "html-writer.hpp"
#include "html-types.hpp"

#include "fspp/filesystem.hpp"
#include "fspp/utils.hpp"

#include <iostream>
#include <ostream>
#include <sstream>


namespace eyestep {

namespace fs = filesystem;

namespace html {

  namespace {
    void escape_str_to_stream(std::ostream& os, const std::string& str,
                              const detail::StyleCtx& ctx) {
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

        case '\n':
          switch (ctx._wrapstyle) {
          case detail::k_asis_wrap:
            os << "<br/>" << std::endl;
            break;
          case detail::k_normal_wrap:
            os << std::endl;
            break;
          case detail::k_no_wrap:
            os << "&nbsp;";
            break;
          }
          break;

        case ' ':
        case '\t':
          switch (ctx._wstreatment) {
          case detail::k_preserve_ws:
            os << "&nbsp;";
            break;
          case detail::k_collapse_ws:
            os << ' ';
            break;
          case detail::k_ignore_ws:
            break;
          }
          break;

        case '\r':
          break;

        default:
          os << c;
        }
      }
    }


    void escape_attr_str_to_stream(std::ostream& os, const std::string& str) {
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


    std::string escape_attr_str(const std::string& str) {
      std::stringstream ss;
      escape_attr_str_to_stream(ss, str);
      return ss.str();
    }
  } // anon ns


  const Doctype k_XHTML_1_0_TRANSITIONAL_DTD =
    {"-//W3C//DTD XHTML 1.0 Transitional//EN",
     "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd"};
  const Doctype k_XHTML_1_1_DTD = {"-//W3C//DTD XHTML 1.1//EN",
                                   "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd"};


  Writer::Writer(const Doctype& doctype, const std::string& generator,
                 const detail::StyleCtx& ctx)
    : _doctype(doctype)
    , _generator(generator)
    , _ctx(&ctx) {}


  void Writer::open(const fs::path& path) {
    _path = path;
    _file = fs::File(path);

    std::error_code ec;
    _file.open(std::ios_base::out | std::ios_base::binary | std::ios_base::trunc, ec);

    if (ec) {
      std::cerr << "Opening file '" << _path << "' failed" << std::endl;
    }
  }


  void Writer::write_attrs(const Attrs& attrs) {
    if (is_open()) {
      for (const auto& attr : attrs) {
        _file.stream() << " " << attr._key << "='";
        escape_attr_str_to_stream(_file.stream(), attr._value);
        _file.stream() << "'";
      }
    }
  }


  void Writer::entity(const std::string& name) {
    if (is_open()) {
      _file.stream() << "&" << name << ";";
    }
  }


  void Writer::open_tag(const std::string& tag, const Attrs& attrs) {
    if (is_open()) {
      _file.stream() << "<" << tag;
      write_attrs(attrs);
      _file.stream() << ">";
    }
  }


  void Writer::close_tag(const std::string& tag) {
    if (is_open()) {
      _file.stream() << "</" << tag << ">";
    }
  }


  void Writer::empty_tag(const std::string& tag, const Attrs& attrs) {
    if (is_open()) {
      _file.stream() << "<" << tag;
      write_attrs(attrs);
      _file.stream() << "/>";
    }
  }


  void Writer::text_tag(const std::string& tag, const std::string& value,
                        const Attrs& attrs) {
    if (is_open()) {
      open_tag(tag, attrs);
      escape_str_to_stream(_file.stream(), value, *_ctx);
      close_tag(tag);
    }
  }


  void Writer::write_text(const std::string& value) {
    if (is_open()) {
      escape_str_to_stream(_file.stream(), value, *_ctx);
    }
  }


  void Writer::write_style(const std::string& text) {
    if (is_open()) {
      open_tag("style", {{"type", "text/css"}});
      newln();
      _file.stream() << text;
      close_tag("style");
      newln();
    }
  }


  void Writer::write_link(const std::string& rel, const Attrs& attrs) {
    if (is_open()) {
      auto new_attrs = attrs;
      new_attrs.insert(new_attrs.begin(), {"rel", rel});

      empty_tag("link", new_attrs);
      newln();
    }
  }


  void Writer::doctype() {
    if (is_open()) {
      _file.stream() << "<!DOCTYPE html PUBLIC '" << _doctype._publicid << "' '"
                     << _doctype._systemid << "'>";
      newln();
    }
  }


  void Writer::header(const std::string& title, const std::string& author,
                      const std::string& desc,
                      const std::function<void(std::ostream&)>& style_proc) {
    if (is_open() && !_has_header) {
      doctype();
      open_tag("html", {Attr{"xmlns", "http://www.w3.org/1999/xhtml"}});
      newln();

      open_tag("head");
      newln();
      text_tag("title", title);
      newln();
      empty_tag("meta",
                {Attr{"http-equiv", "Content-Type"},
                 Attr{"content", "text/html; charset=UTF-8"}});
      newln();
      if (!author.empty()) {
        empty_tag("meta",
                  {Attr{"name", "author"}, Attr{"content", escape_attr_str(author)}});
        newln();
      }
      if (!desc.empty()) {
        empty_tag("meta",
                  {Attr{"name", "description"}, Attr{"content", escape_attr_str(desc)}});
        newln();
      }

      empty_tag("meta", {Attr{"name", "generator"}, Attr{"content", _generator}});
      newln();

      style_proc(_file.stream());

      close_tag("head");
      newln();
      open_tag("body");
      newln();

      _has_header = true;
    }
  }


  void Writer::footer() {
    if (is_open()) {
      close_tag("body");
      newln();
      close_tag("html");
      newln();
    }
  }


  void Writer::newln() {
    if (is_open()) {
      _file.stream() << std::endl;
    }
  }


  //----------------------------------------------------------------------------

  CSSWriter::CSSWriter(const std::string& generator)
    : _generator(generator) {}


  void CSSWriter::open(const filesystem::path& path) {
    _path = path;
    _file = fs::File(path);

    std::error_code ec;
    _file.open(std::ios_base::out | std::ios_base::binary | std::ios_base::trunc, ec);

    if (ec) {
      std::cerr << "Opening file '" << _path << "' failed" << std::endl;
    }
    else {
      _file.stream() << "/* generated by: " << _generator << " */" << std::endl;
    }

    // write some defaults for the "p" tag to overwrite settings from the
    // browser.
    _file.stream() << "p {" << std::endl
                   << "  margin: 0px 0px 0px 0px;" << std::endl
                   << "  margin-top: 0px;" << std::endl
                   << "  margin-left: 0px;" << std::endl
                   << "  margin-bottom: 0px;" << std::endl
                   << "  margin-right: 0px;" << std::endl
                   << "  padding-left: 0px;" << std::endl
                   << "  padding-right: 0px;" << std::endl
                   << "  padding-top: 0px;" << std::endl
                   << "  padding-bottom: 0px;" << std::endl
                   << "}" << std::endl
                   << "a {" << std::endl
                   << "  text-decoration: none;" << std::endl
                   << "  color: black;" << std::endl
                   << "}" << std::endl
                   << "body {" << std::endl
                   << "  margin: 0px;" << std::endl
                   << "}" << std::endl;
  }


  void CSSWriter::write_rule(const std::string& selector, const std::string& props) {
    if (is_open()) {
      _file.stream() << selector << " {" << std::endl
                     << props << std::endl
                     << "}" << std::endl
                     << std::endl;
    }
  }


  std::string CSSWriter::add_rule(const std::string& tag, const std::string& props) {
    if (is_open()) {
      auto key = tag + "~~" + props;
      auto i_find = _props_cache.find(key);

      if (i_find == _props_cache.end()) {
        static int counter = 0;

        auto auto_class = std::string("auto-").append(std::to_string(++counter));
        auto selector = tag + "." + auto_class;

        write_rule(selector, props);

        _props_cache[key] = auto_class;
        return auto_class;
      }
      else
        return i_find->second;
    }

    return {};
  }
} // ns html
} // ns eyestep
