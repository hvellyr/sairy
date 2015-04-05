// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"
#include "textbook-model.hpp"
#include "textbook-parser.hpp"

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/range/adaptor/transformed.hpp>

#include <algorithm>
#include <cassert>
#include <exception>
#include <iostream>
#include <map>
#include <unordered_map>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>


namespace eyestep {
namespace textbook {

  const std::string k_attrs_tag = "attrs";
  const std::string k_c_tag = "c";
  const std::string k_textbook_tag = "textbook";
  const std::string k_end_tag = "end";
  const std::string k_include_tag = "include";
  const std::string k_let_tag = "let";
  const std::string k_model_tag = "model";
  const std::string k_p_tag = "p";
  const std::string k_tag_tag = "tag";
  const std::string k_version_tag = "version";

  const std::string k_ANY_opt = "ANY";
  const std::string k_EMPTY_opt = "EMPTY";
  const std::string k_FREF_opt = "FREF";
  const std::string k_ID_opt = "ID";
  const std::string k_NOP_opt = "NOP";
  const std::string k_P_opt = "P";
  const std::string k_TEXT_opt = "#TEXT";


  namespace fs = boost::filesystem;

  namespace {
    std::string load_file_into_string(const fs::path& path)
    {
      std::stringstream result;

      fs::ifstream in(path, std::ios::in | std::ios::binary);
      in.exceptions(std::ifstream::failbit);

      result << in.rdbuf();

      return result.str();
    }

    fs::path path_rel_to_dir(const fs::path& basepath, const fs::path& fpath)
    {
      if (!fpath.string().empty()) {
        if (fpath.is_absolute()) {
          return fpath;
        }

        return basepath.parent_path() /= fpath;
      }

      return fs::path();
    }

    boost::optional<fs::path>
    find_model_spec(const std::string& tag,
                    const std::vector<fs::path>& catalog_search_path)
    {
      for (auto loc : catalog_search_path) {
        auto path = (loc /= tag).replace_extension(".spec");

        if (fs::exists(path) && fs::is_regular_file(path)) {
          return path;
        }
      }

      return boost::none;
    }


    std::string node_data(const Node* nd)
    {
      return nd->property<std::string>(CommonProps::k_data);
    }


    std::vector<std::string> split_attrs(const std::string& str)
    {
      using namespace boost::adaptors;
      using namespace boost::algorithm;

      std::vector<std::string> steps;
      split(steps, str, is_any_of(","), token_compress_on);

      return boost::copy_range<std::vector<std::string>>(
        steps | transformed([](const std::string& value) {
          return boost::trim_copy(value);
        }));
    }


    std::unique_ptr<DocSpec> make_docspec(const Node* nd)
    {
      auto docspec = estd::make_unique<DocSpec>();

      for (const auto* tagnd : nd->property<Nodes>(CommonProps::k_children)) {
        if (tagnd->gi() == k_tag_tag) {
          auto attrs = tagnd->property<Nodes>(CommonProps::k_attrs);

          if (attrs.size() >= 3) {
            auto gi_spec = boost::trim_copy(node_data(attrs[0]));
            auto attr_spec = boost::trim_copy(node_data(attrs[1]));
            auto body_spec = boost::trim_copy(node_data(attrs[2]));
            auto opt = attrs.size() >= 4 ? boost::trim_copy(node_data(attrs[3]))
                                         : std::string();

            size_t min_attr = 0;
            size_t max_attr = 0;

            std::vector<AttrSpec> attrspecs;
            for (const auto& attr : split_attrs(attr_spec)) {
              if (!attr.empty()) {
                bool is_opt = true;
                std::string nm;
                AttrType ty;

                if (attr.back() == '?') {
                  is_opt = true;
                  nm = attr.substr(0, attr.size() - 1);
                }
                else {
                  min_attr += 1;
                  is_opt = false;
                  nm = attr;
                }
                max_attr += 1;

                if (nm == k_ID_opt) {
                  ty = k_attr_id;
                }
                else if (nm == k_FREF_opt) {
                  ty = k_attr_fref;
                }
                else {
                  ty = k_attr_str;
                }

                attrspecs.push_back(AttrSpec(nm, ty, is_opt));
              }
            }

            bool is_env = body_spec != k_EMPTY_opt;
            bool is_mixed_content =
              body_spec.find(k_TEXT_opt) != std::string::npos;
            bool starts_p = opt.find(k_P_opt) != std::string::npos;
            bool is_block = opt.find(k_NOP_opt) != std::string::npos;

            auto tag_spec =
              TagSpec(gi_spec, attrspecs, std::make_tuple(min_attr, max_attr),
                      is_env, is_mixed_content, starts_p, is_block);
            docspec->add(tag_spec);
          }
        }
      }

      return std::move(docspec);
    }


    std::unique_ptr<DocSpec> model_doc_type_doc_spec()
    {
      auto docspec = estd::make_unique<DocSpec>();

      docspec->add(
        TagSpec(k_tag_tag,
                std::vector<AttrSpec>{AttrSpec(k_tag_tag, k_attr_str, false),
                                      AttrSpec(k_attrs_tag, k_attr_str, false),
                                      AttrSpec(k_model_tag, k_attr_str, false),
                                      AttrSpec(k_p_tag, k_attr_str, true)},
                std::make_tuple(3u, 4u),
                false, // is_env
                false  // is_mixed
                ));
      docspec->add(TagSpec(k_textbook_tag,
                           std::vector<AttrSpec>{
                             AttrSpec(k_version_tag, k_attr_str, false)},
                           std::make_tuple(1u, 1u),
                           false, // is_env,
                           false  // is_mixed
                           ));

      return std::move(docspec);
    }


    std::unique_ptr<DocSpec> read_model(const fs::path& path,
                                        const std::vector<fs::path>& catalog_path,
                                        bool is_verbose = false)
    {
      eyestep::Grove grove;
      GroveBuilder gb(grove.make_node(document_class_definition()));

      Catalog catalog;
      catalog[k_textbook_tag] = model_doc_type_doc_spec();

      VariableEnv vars;

      Parser p(grove, gb, vars, catalog, nullptr,
               catalog_path,
               false, // mixed content
               is_verbose);

      auto nd = p.parse_file(path);
      if (is_verbose) {
        std::cerr << "DOCTYPE: " << *nd << std::endl;
      }

      if (p.doctype() == k_textbook_tag) {
        auto children = nd->property<Nodes>(CommonProps::k_children);
        if (!children.empty()) {
          auto textbook_nd = children[0];
          if (textbook_nd) {
            auto attrs = textbook_nd->property<Nodes>(CommonProps::k_attrs);
            if (!attrs.empty() &&
                boost::trim_copy(node_data(attrs[0])) == "1.0") {
              return make_docspec(nd);
            }
          }
        }
      }

      return nullptr;
    }

  } // anon namespace


  //------------------------------------------------------------------------------

  Stream::Stream(boost::optional<std::string> data,
                 boost::optional<fs::path> path, size_t start_line_no)
    : _unread_nc(0u), _current_c(' '), _nc(0u), _line_no(start_line_no),
      _fpath(path ? *path : fs::path("<data>")),
      _data(data ? *data : (path ? load_file_into_string(*path) : ""))
  {
    if (_nc < _data.size()) {
      _current_c = _data[_nc];
    }
    else {
      _current_c = ' ';
    }
  }

  char Stream::current_c() const { return _current_c; }

  size_t Stream::line_no() const { return _line_no; }

  fs::path Stream::fpath() const { return _fpath; }

  std::string Stream::srcpos() const
  {
    std::stringstream ss;
    ss << _fpath << ":" << _line_no;
    return ss.str();
  }


  char Stream::read()
  {
    if (_unread_nc > 0) {
      _unread_nc--;
      _current_c = _unread_buffer[_unread_nc];
    }
    else {
      _nc++;
      if (_nc < _data.size()) {
        _current_c = _data[_nc];
        if (_current_c == '\n') {
          _line_no++;
        }
      }
      else {
        _current_c = ' ';
      }
    }

    return _current_c;
  }

  char Stream::unread(const std::string& buf)
  {
    if (!buf.empty()) {
      if (buf.size() > _unread_buffer.size()) {
        _unread_buffer.resize(buf.size());
      }

      auto rbuf = buf;
      std::reverse(rbuf.begin(), rbuf.end());

      for (const auto c : rbuf) {
        _unread_buffer[_unread_nc++] = c;
      }

      _nc--;
      if (_current_c == '\n') {
        _line_no--;
      }

      return read();
    }

    return current_c();
  }

  bool Stream::eof() const { return _nc >= _data.size() && _unread_nc == 0; }


  //------------------------------------------------------------------------------

  GroveBuilder::GroveBuilder(Node* rootnd)
    : _current_node(rootnd), _root_node(rootnd)
  {
  }

  void GroveBuilder::add_node(Node* nd) { _current_node->add_child_node(nd); }

  void GroveBuilder::push_node(const std::string& tag, Node* nd)
  {
    add_node(nd);

    _node_stack.push_front(
      std::tuple<std::string, Node*>{_current_tag, _current_node});
    _current_node = nd;
    _current_tag = tag;
  }

  void GroveBuilder::pop_node(const std::string& tag)
  {
    if (_current_tag != tag) {
      std::stringstream ss;
      ss << "Close tag not open: " << tag << " (current tag: " << _current_tag
         << ")";
      throw ParseException("", ss.str());
    }

    auto tup = _node_stack.front();
    _node_stack.pop_front();

    _current_node = std::get<1>(tup);
    _current_tag = std::get<0>(tup);

    if (_node_stack.empty()) {
      if (_current_node != _root_node) {
        throw ParseException("", "Broken node hierarchy");
      }
    }
  }

  Node* GroveBuilder::root_node() { return _root_node; }


  //------------------------------------------------------------------------------


  const std::set<char> SYMBOL1 = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
                                  'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
                                  'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
                                  'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F',
                                  'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
                                  'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
                                  'W', 'X', 'Y', 'Z', '_', '?', '*'};

  const std::set<char> SYMBOLn = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
                                  'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
                                  's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A',
                                  'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
                                  'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
                                  'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '_', '?',
                                  '*', '0', '1', '2', '3', '4', '5', '6', '7',
                                  '8', '9'};
  const std::set<char> WS = {' ', '\t', '\r', '\n'};

  bool is_symbol1(char c) { return SYMBOL1.find(c) != SYMBOL1.end(); }

  bool is_symbol_n(char c) { return SYMBOLn.find(c) != SYMBOLn.end(); }

  bool is_ws(char c) { return WS.find(c) != WS.end(); }

  Parser::Parser(eyestep::Grove& grove, GroveBuilder& grovebuilder,
                 VariableEnv& vars, Catalog& catalog, DocSpec* docspec,
                 const std::vector<fs::path>& catalog_path,
                 bool is_mixed_content, bool is_verbose)
    : _verbose(is_verbose), _catalog_path(catalog_path),
      _grove(grove), _grovebuilder(grovebuilder), _vars(vars),
      _catalog(catalog), _docspec(docspec), _current_p_nd(nullptr),
      _is_mixed_content(is_mixed_content)
  {
  }

  std::string Parser::doctype() const { return _doctype; }

  void Parser::push_stream(std::shared_ptr<Stream> stream)
  {
    _stream_stack.push_front(_stream);
    _stream = stream;
  }

  void Parser::pop_stream()
  {
    _stream = _stream_stack.front();
    _stream_stack.pop_front();
  }

  bool Parser::eof() { return _stream->eof() && _stream_stack.empty(); }

  char Parser::nextc()
  {
    while (_stream->eof() && !_stream_stack.empty()) {
      pop_stream();
    }
    return _stream->read();
  }

  char Parser::unread(const std::string& buf) { return _stream->unread(buf); }

  char Parser::currentc() { return _stream->current_c(); }

  size_t Parser::lineno() { return _stream->line_no(); }

  std::string Parser::srcpos() { return _stream->srcpos(); }


  Node* Parser::parse_file(const fs::path& fpath)
  {
    return parse_stream(std::make_shared<Stream>(boost::none, fpath));
  }

  Node* Parser::parse_string(const std::string& buf, size_t start_line_no)
  {
    return parse_stream(
      std::make_shared<Stream>(buf, boost::none, start_line_no));
  }

  Node* Parser::parse_stream(std::shared_ptr<Stream> stream)
  {
    _stream = stream;

    while (!eof()) {
      char c = currentc();

      if (c == '@') {
        parse_at_form();
      }
      else if (c == '\n') {
        auto text = _text._stream.str();

        if (!text.empty() && text.back() == '\n') {
          finish_paragraph();
          skip_ws();
        }
        else {
          _text._stream << c;
          nextc();
        }
      }
      else {
        _text._stream << c;
        nextc();
      }
    }

    finish_paragraph();

    return _grovebuilder.root_node();
  }


  void Parser::push_p_nd()
  {
    if (_is_mixed_content) {
      if (!_current_p_nd) {
        _current_p_nd = _grove.make_elt_node(k_p_tag);
        _grovebuilder.push_node(k_p_tag, _current_p_nd);
      }
    }
  }


  void Parser::push_text()
  {
    if (_is_mixed_content) {
      auto text = _text._stream.str();
      if (!text.empty()) {
        push_p_nd();
        _grovebuilder.add_node(_grove.make_text_node(text));

        _text = TextBuffer();
      }
    }
    else {
      _text = TextBuffer();
    }
  }


  void Parser::finish_paragraph()
  {
    push_text();

    if (_current_p_nd) {
      _grovebuilder.pop_node(k_p_tag);
      _current_p_nd = nullptr;
    }
  }

  std::string Parser::skip_ws()
  {
    std::stringstream tmp;

    while (!eof() && is_ws(currentc())) {
      tmp << currentc();
      nextc();
    }

    return tmp.str();
  }


  void Parser::skip_until_eol()
  {
    while (!eof() && currentc() != '\n') {
      nextc();
    }
    if (currentc() == '\n') {
      nextc();
    }
  }


  std::string Parser::parse_symbol()
  {
    std::stringstream sym;
    while (!eof() && is_symbol_n(currentc())) {
      sym << currentc();
      nextc();
    }
    return sym.str();
  }


  Parser::Args Parser::parse_args()
  {
    Args args;
    std::string skipped_ws;

    while (!eof() && currentc() == '{') {
      nextc();

      std::stringstream arg;
      bool done = false;
      int bracecount = 0;
      std::list<std::string> brace_srcpos = {srcpos()};

      while (!done && !eof()) {
        auto sp = srcpos();
        char c = currentc();
        nextc();

        switch (c) {
        case '@':
          c = currentc();
          if (c == '{' || c == '}') {
            arg << '@';
            arg << c;
            nextc();
          }
          else {
            arg << '@';
            arg << c;
            nextc();
          }
          break;
        case '{':
          bracecount++;
          brace_srcpos.push_front(sp);
          arg << c;
          break;
        case '}':
          bracecount--;
          brace_srcpos.pop_front();
          if (bracecount < 0) {
            done = true;
          }
          else {
            arg << c;
          }
          break;
        default:
          arg << c;
        }
      }

      if (eof() && bracecount >= 0) {
        throw ParseException(srcpos(), "Unclosed {");
      }

      args.push_back(boost::trim_copy(arg.str()));
      skipped_ws = skip_ws();
    }

    unread(skipped_ws);

    return args;
  }


  Parser::AttrsAndId Parser::args2nl(const Args& args, const TagSpec& tagspec,
                                     size_t lineno_at_start)
  {
    Nodes nl;
    boost::optional<std::string> idstr;

    const auto& attrspecs = tagspec.attrspecs();
    size_t attrc = 0;

    for (const auto& arg : args) {
      // don't reparse argument if ID type
      if (attrspecs[attrc].type() == k_attr_id) {
        if (!idstr) {
          idstr = boost::trim_copy(arg);
        }
      }
      else if (attrspecs[attrc].type() == k_attr_fref) {
        auto abspath = path_rel_to_dir(_stream->fpath(), boost::trim_copy(arg));
        nl.push_back(_grove.make_text_node(abspath.string()));
      }
      else {
        GroveBuilder gb(_grove.make_elt_node("<root>"));
        Parser p(_grove, gb, _vars, _catalog, _docspec, _catalog_path, true);
        auto nd = p.parse_stream(
          std::make_shared<Stream>(arg, _stream->fpath(), lineno_at_start));

        auto children = nd->property<Nodes>(CommonProps::k_children);
        if (children.size() == 1u && children[0]->gi() == k_p_tag) {
          auto grandch = children[0]->property<Nodes>(CommonProps::k_children);
          unparent_nodes(grandch);

          for (auto* child : grandch) {
            child->set_property(CommonProps::k_attr_name,
                                attrspecs[attrc].name());
            nl.push_back(child);
          }
        }
        else if (children.empty()) {
          nl.push_back(_grove.make_text_node(""));
        }
        else {
          unparent_nodes(children);

          for (auto* child : children) {
            child->set_property(CommonProps::k_attr_name,
                                attrspecs[attrc].name());
            nl.push_back(child);
          }
        }

        _grove.remove_node(nd);
      }

      attrc++;
    }

#ifndef NDEBUG
    for (auto* nd : nl) {
      assert(nd->parent() == nullptr);
    }
#endif

    return AttrsAndId{nl, idstr};
  }

  void Parser::parse_at_form()
  {
    assert(currentc() == '@');

    nextc();

    char c = currentc();

    switch (c) {
    case '@': // @@
      parse_at();
      break;
    case '{':
      parse_openbrace();
      break;
    case '}':
      parse_closebrace();
      break;
    case '=':
      push_text();
      parse_varlookup();
      break;
    case '-':
      // NOP
      break;
    default:
      push_text();
      if (is_symbol1(c)) { // @abc
        parse_tag();
      }
      // else: NOP
      break;
    }
  }

  void Parser::parse_at()
  {
    _text._stream << '@';
    nextc();
  }

  void Parser::parse_openbrace()
  {
    _text._stream << '{';
    nextc();
  }

  void Parser::parse_closebrace()
  {
    _text._stream << '}';
    nextc();
  }

  void Parser::parse_varlookup()
  {
    nextc();

    auto args = parse_args();
    if (args.size() != 1u) {
      throw ParseException(srcpos(), "Wrong number of args to = expression");
    }

    auto value = _vars[args.front()];
    unread(value);
  }


  void Parser::end_tag(const std::string& endtag)
  {
    finish_paragraph();
    _grovebuilder.pop_node(endtag);
    skip_ws();

    _is_mixed_content = _is_mixed_content_stack.front();
    _is_mixed_content_stack.pop_front();
  }

  void Parser::parse_endtag()
  {
    skip_ws();

    if (currentc() == '{') {
      auto args = parse_args();

      if (args.size() == 1) {
        if (args[0] != k_p_tag) {
          end_tag(boost::trim_copy(args[0]));
        }
      }
      else {
        throw ParseException(srcpos(), "Expected end qualifier");
      }
    }
    else if (is_symbol1(currentc())) {
      auto endtag = parse_symbol();
      if (endtag != k_p_tag) {
        end_tag(endtag);
      }
    }
    else {
      throw ParseException(srcpos(), "Expected end qualifier");
    }
  }

  void Parser::parse_let()
  {
    auto args = parse_args();
    if (args.size() != 2u) {
      throw ParseException(srcpos(), "Wrong number of args to let expression");
    }
    _vars[args[0]] = args[1];
  }

  fs::path Parser::path_for_include(const fs::path& fpath)
  {
    if (!fpath.string().empty()) {
      if (fpath.is_absolute()) {
        return fpath;
      }

      auto currentfp = _stream->fpath();
      return currentfp.parent_path() /= fpath;
    }
    return fs::path();
  }


  void Parser::parse_include()
  {
    auto sp = srcpos();
    auto args = parse_args();

    if (args.size() != 1u) {
      throw ParseException(srcpos(),
                           "Wrong number of args to include expression");
    }

    try {
      auto inclfp = path_for_include(args.front());
      if (_verbose) {
        std::cerr << "include: " << inclfp << std::endl;
      }

      auto stream = std::make_shared<Stream>(boost::none, inclfp);
      push_stream(stream);
    }
    catch (const ParseException& pe) {
      std::cerr << srcpos() << ": Error while parsing include file '"
                << args.front() << "'" << std::endl;
      std::cerr << pe.what() << std::endl;
    }
  }


  void Parser::parse_tag_with_params(const std::string& tag)
  {
    bool is_root_tag = false;
    if (!_docspec) {
      is_root_tag = true;
      set_docspec_by_root_tag(tag);
    }

    auto lineno_at_start = lineno();
    auto args = parse_args();

    auto tagspec = _docspec->lookup(tag);
    if (tagspec) {
      check_tag_args(args, *tagspec);

      auto attrs_and_id = args2nl(args, *tagspec, lineno_at_start);
      auto nl = std::get<0>(attrs_and_id);
      auto idstr = std::get<1>(attrs_and_id);

      if (tagspec->is_env()) {
        finish_paragraph();

        // Don't create explicit "@p" tags
        if (tag != k_p_tag) {
          auto nd = _grove.make_elt_node(tag);
          nd->set_property(CommonProps::k_attrs, nl);
          if (idstr) {
            nd->set_property(CommonProps::k_id, *idstr);
          }

          _grovebuilder.push_node(tag, nd);
          _is_mixed_content_stack.push_front(_is_mixed_content);
          _is_mixed_content = tagspec->is_mixed();
        }

        skip_ws();
      }
      else if (tagspec->is_block()) {
        finish_paragraph();

        auto nd = _grove.make_elt_node(tag);
        nd->set_property(CommonProps::k_attrs, nl);
        if (idstr) {
          nd->set_property(CommonProps::k_id, *idstr);
        }

        _grovebuilder.add_node(nd);
        skip_ws();
      }
      else {
        if (!is_root_tag) {
          if (tagspec->starts_p()) {
            finish_paragraph();
          }
          push_p_nd();
        }

        auto nd = _grove.make_elt_node(tag);
        nd->set_property(CommonProps::k_attrs, nl);
        if (idstr) {
          nd->set_property(CommonProps::k_id, *idstr);
        }

        _grovebuilder.add_node(nd);
      }
    }
    else {
      throw ParseException(srcpos(), "Unknown tag: " + tag);
    }
  }

  void Parser::parse_tag()
  {
    auto tag = parse_symbol();

    if (tag == k_end_tag) { // @end abc
      parse_endtag();
    }
    else if (tag == k_c_tag) { // @c
      skip_until_eol();
    }
    else if (tag == k_let_tag) {
      parse_let();
    }
    else if (tag == k_include_tag) {
      parse_include();
    }
    else { // @abc{xyz}{nmo}
      parse_tag_with_params(tag);
    }
  }


  void Parser::set_docspec_by_root_tag(const std::string& tag)
  {
    if (_catalog.find(tag) != _catalog.end()) {
      _docspec = _catalog[tag].get();
      _doctype = tag;
    }
    else {
      auto path = find_model_spec(tag, _catalog_path);
      if (path) {
        auto docspec = read_model(*path, _catalog_path, _verbose);
        if (docspec.get()) {
          _catalog[tag] = std::move(docspec);
          _docspec = _catalog[tag].get();
          _doctype = tag;
        }
        else {
          throw ParseException("", std::string("Can't read document spec: ") +
                                     path->string());
        }
      }
      else {
        throw ParseException("", std::string("Unknown root tag: ") + tag);
      }
    }
  }


  void Parser::check_tag_args(const Parser::Args& args, const TagSpec& tagspec)
  {
    if (args.size() > tagspec.max_args()) {
      std::stringstream ss;
      ss << "Too many arguments to tag: " << tagspec.tag_name() << "(max. "
         << tagspec.max_args() << " expected, " << args.size() << " found)";
      throw ParseException(srcpos(), ss.str());
    }
    else if (args.size() < tagspec.min_args()) {
      std::stringstream ss;
      ss << "Not enough arguments to tag: " << tagspec.tag_name() << "("
         << tagspec.min_args() << " expected, " << args.size() << " found)";
      throw ParseException(srcpos(), ss.str());
    }
  }

} // ns textbook
} // ns eyestep
