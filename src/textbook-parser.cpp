// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "textbook-parser.hpp"
#include "estd/memory.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"
#include "nodeutils.hpp"
#include "textbook-model.hpp"
#include "utils.hpp"

#include "fspp/estd/optional.hpp"
#include "fspp/filesystem.hpp"
#include "fspp/utils.hpp"

#include <algorithm>
#include <cassert>
#include <exception>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>


namespace eyestep {
namespace textbook {

  const auto k_attrs_tag = std::string("attrs");
  const auto k_c_tag = std::string("c");
  const auto k_textbook_tag = std::string("textbook");
  const auto k_end_tag = std::string("end");
  const auto k_include_tag = std::string("include");
  const auto k_let_tag = std::string("let");
  const auto k_model_tag = std::string("model");
  const auto k_p_tag = std::string("p");
  const auto k_tag_tag = std::string("tag");
  const auto k_version_tag = std::string("version");

  const auto k_ANY_opt = std::string("ANY");
  const auto k_EMPTY_opt = std::string("EMPTY");
  const auto k_FREF_opt = std::string("FREF");
  const auto k_ID_opt = std::string("ID");
  const auto k_NOP_opt = std::string("NOP");
  const auto k_P_opt = std::string("P");
  const auto k_TEXT_opt = std::string("#TEXT");


  namespace fs = filesystem;

  namespace {
    std::string load_file_into_string(const fs::path& path) {
      std::stringstream result;

      std::error_code ec;
      fs::with_stream(path, std::ios::in | std::ios::binary, ec,
                      [&](std::istream& in) { result << in.rdbuf(); });

      return result.str();
    }


    fs::path path_rel_abs(const fs::path& basepath, const fs::path& fpath) {
      if (!fpath.string().empty()) {
        if (fpath.is_absolute()) {
          return fpath;
        }

        return basepath.parent_path() /= fpath;
        // return utils::make_relative(fs::current_path(), abspath);
      }

      return {};
    }

    estd::optional<fs::path>
    find_model_spec(const std::string& tag,
                    const std::vector<fs::path>& catalog_search_path,
                    const fs::path& extension) {
      for (auto loc : catalog_search_path) {
        auto path = (loc /= tag).replace_extension(extension);

        if (fs::exists(path) && fs::is_regular_file(path)) {
          return path;
        }
      }

      return {};
    }


    std::vector<std::string> split_attrs(const std::string& str) {
      return utils::split(str, ",", true);
    }


    std::unique_ptr<DocSpec> make_docspec(const Node* nd) {
      auto docspec = estd::make_unique<DocSpec>();

      for (const auto* tagnd : nd->property<Nodes>(CommonProps::k_children)) {
        if (tagnd->gi() == k_tag_tag) {
          auto attrs = tagnd->attributes();

          if (attrs.size() >= 3) {
            auto gi_spec = utils::trim_copy(node_data(attrs[0]));
            auto attr_spec = utils::trim_copy(node_data(attrs[1]));
            auto body_spec = utils::trim_copy(node_data(attrs[2]));
            auto opt =
              attrs.size() >= 4 ? utils::trim_copy(node_data(attrs[3])) : std::string();

            auto min_attr = 0u;
            auto max_attr = 0u;

            auto attrspecs = std::vector<AttrSpec>{};
            for (const auto& attr : split_attrs(attr_spec)) {
              if (!attr.empty()) {
                auto is_opt = true;
                auto is_data = false;
                auto nm = std::string{};
                auto ty = AttrType{};

                if (attr.back() == '?') {
                  is_opt = true;
                  nm = attr.substr(0, attr.size() - 1);
                }
                else {
                  min_attr += 1;
                  is_opt = false;
                  nm = attr;
                }

                if (nm.front() == '%') {
                  is_data = true;
                  nm = nm.substr(1, attr.size());
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

                attrspecs.push_back(AttrSpec(nm, ty, is_opt, is_data));
              }
            }

            auto is_env = body_spec != k_EMPTY_opt;
            auto is_mixed_content = body_spec.find(k_TEXT_opt) != std::string::npos;
            auto starts_p = opt.find(k_P_opt) != std::string::npos;
            auto is_block = opt.find(k_NOP_opt) != std::string::npos;

            auto tag_spec =
              TagSpec(gi_spec, attrspecs, std::make_tuple(min_attr, max_attr), is_env,
                      is_mixed_content, starts_p, is_block);
            docspec->add(tag_spec);
          }
        }
      }

      return docspec;
    }


    std::unique_ptr<DocSpec> model_doc_type_doc_spec() {
      auto docspec = estd::make_unique<DocSpec>();

      docspec->add(TagSpec(k_tag_tag,
                           {
                             AttrSpec(k_tag_tag, k_attr_str, false, false),
                             AttrSpec(k_attrs_tag, k_attr_str, false, false),
                             AttrSpec(k_model_tag, k_attr_str, false, false),
                             AttrSpec(k_p_tag, k_attr_str, true, false),
                           },
                           std::make_tuple(3u, 4u),
                           false, // is_env
                           false  // is_mixed
                           ));
      docspec->add(TagSpec(k_textbook_tag,
                           {AttrSpec(k_version_tag, k_attr_str, false, false)},
                           std::make_tuple(1u, 1u),
                           false, // is_env,
                           false  // is_mixed
                           ));

      return docspec;
    }


    std::unique_ptr<DocSpec> read_model(const fs::path& path,
                                        const std::vector<fs::path>& catalog_path,
                                        bool is_verbose = false) {
      auto grove = eyestep::Grove{};
      auto gb = GroveBuilder(grove.make_node(document_class_definition()));

      auto catalog = Catalog{};
      catalog[k_textbook_tag] = model_doc_type_doc_spec();

      auto vars = VariableEnv{};

      auto p = Parser(grove, gb, vars, catalog, nullptr, catalog_path,
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
            auto attrs = textbook_nd->attributes();
            if (!attrs.empty() && utils::trim_copy(node_data(attrs[0])) == "1.0") {
              return make_docspec(nd);
            }
          }
        }
      }

      return nullptr;
    }

  } // namespace


  //------------------------------------------------------------------------------

  Stream::Stream(estd::optional<std::string> data, estd::optional<fs::path> path,
                 size_t start_line_no)
    : _unread_nc(0u)
    , _current_c(' ')
    , _nc(0u)
    , _line_no(start_line_no)
    , _fpath(path ? *path : fs::path("<data>"))
    , _data(data ? *data : (path ? load_file_into_string(*path) : "")) {
    if (_nc < _data.size()) {
      _current_c = _data[_nc];
    }
    else {
      _current_c = ' ';
    }
  }


  char Stream::current_c() const {
    return _current_c;
  }


  size_t Stream::line_no() const {
    return _line_no;
  }


  fs::path Stream::fpath() const {
    return _fpath;
  }


  std::string Stream::srcpos() const {
    std::stringstream ss;
    ss << _fpath << ":" << _line_no;
    return ss.str();
  }


  char Stream::read() {
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


  char Stream::unread(const std::string& buf) {
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


  bool Stream::eof() const {
    return _nc >= _data.size() && _unread_nc == 0;
  }


  //------------------------------------------------------------------------------

  GroveBuilder::GroveBuilder(Node* rootnd)
    : _current_node(rootnd)
    , _root_node(rootnd) {}


  void GroveBuilder::add_node(Node* nd) {
    _current_node->add_child_node(nd);
  }


  void GroveBuilder::push_node(const std::string& tag, Node* nd) {
    add_node(nd);

    _node_stack.push_front(std::tuple<std::string, Node*>{_current_tag, _current_node});
    _current_node = nd;
    _current_tag = tag;
  }


  void GroveBuilder::pop_node(const std::string& tag) {
    if (_current_tag != tag) {
      std::stringstream ss;
      ss << "Close tag not open: " << tag << " (current tag: " << _current_tag << ")";
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


  Node* GroveBuilder::root_node() {
    return _root_node;
  }


  //------------------------------------------------------------------------------

  const auto SYMBOL1 = std::set<char>{
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B',
    'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '_', '?', '*',
  };
  const auto SYMBOLn = std::set<char>{
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q',
    'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y',
    'Z', '_', '?', '*', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  };
  const auto WS = std::set<char>{' ', '\t', '\r', '\n'};


  bool is_symbol1(char c) {
    return SYMBOL1.find(c) != SYMBOL1.end();
  }


  bool is_symbol_n(char c) {
    return SYMBOLn.find(c) != SYMBOLn.end();
  }


  bool is_ws(char c) {
    return WS.find(c) != WS.end();
  }


  Parser::Parser(eyestep::Grove& grove, GroveBuilder& grovebuilder, VariableEnv& vars,
                 Catalog& catalog, DocSpec* docspec,
                 const std::vector<fs::path>& catalog_path, bool is_mixed_content,
                 bool is_verbose)
    : _verbose(is_verbose)
    , _catalog_path(catalog_path)
    , _text(estd::make_unique<std::stringstream>())
    , _grove(grove)
    , _grovebuilder(grovebuilder)
    , _vars(vars)
    , _catalog(catalog)
    , _docspec(docspec)
    , _current_p_nd(nullptr)
    , _is_mixed_content(is_mixed_content) {}


  std::string Parser::doctype() const {
    return _doctype;
  }


  void Parser::push_stream(std::shared_ptr<Stream> stream) {
    _stream_stack.push_front(_stream);
    _stream = stream;
  }


  void Parser::pop_stream() {
    _stream = _stream_stack.front();
    _stream_stack.pop_front();
  }


  bool Parser::eof() {
    return _stream->eof() && _stream_stack.empty();
  }


  char Parser::nextc() {
    while (_stream->eof() && !_stream_stack.empty()) {
      pop_stream();
    }
    return _stream->read();
  }


  char Parser::unread(const std::string& buf) {
    return _stream->unread(buf);
  }


  char Parser::currentc() {
    return _stream->current_c();
  }


  size_t Parser::lineno() {
    return _stream->line_no();
  }


  std::string Parser::srcpos() {
    return _stream->srcpos();
  }


  Node* Parser::parse_file(const fs::path& fpath) {
    return parse_stream(std::make_shared<Stream>(estd::optional<std::string>{}, fpath));
  }


  Node* Parser::parse_string(const std::string& buf, size_t start_line_no) {
    return parse_stream(
      std::make_shared<Stream>(buf, estd::optional<filesystem::path>{}, start_line_no));
  }


  Node* Parser::parse_stream(std::shared_ptr<Stream> stream) {
    _stream = stream;

    while (!eof()) {
      auto c = currentc();

      if (c == '@') {
        parse_at_form();
      }
      else if (c == '\n') {
        auto text = _text->str();

        if (!text.empty() && text.back() == '\n') {
          finish_paragraph();
          skip_ws();
        }
        else {
          *_text << c;
          nextc();
        }
      }
      else {
        *_text << c;
        nextc();
      }
    }

    finish_paragraph();

    return _grovebuilder.root_node();
  }


  void Parser::push_p_nd() {
    if (_is_mixed_content) {
      if (!_current_p_nd) {
        _current_p_nd = _grove.make_elt_node(k_p_tag);
        _grovebuilder.push_node(k_p_tag, _current_p_nd);
      }
    }
  }


  void Parser::push_text() {
    if (_is_mixed_content) {
      auto text = _text->str();
      if (!text.empty()) {
        push_p_nd();
        _grovebuilder.add_node(_grove.make_text_node(text));

        _text->str(std::string());
      }
    }
    else {
      _text->str(std::string());
    }
  }


  void Parser::finish_paragraph() {
    push_text();

    if (_current_p_nd) {
      _grovebuilder.pop_node(k_p_tag);
      _current_p_nd = nullptr;
    }
  }


  std::string Parser::skip_ws() {
    std::stringstream tmp;

    while (!eof() && is_ws(currentc())) {
      tmp << currentc();
      nextc();
    }

    return tmp.str();
  }


  void Parser::skip_until_eol() {
    while (!eof() && currentc() != '\n') {
      nextc();
    }
    if (currentc() == '\n') {
      nextc();
    }
  }


  std::string Parser::parse_symbol() {
    std::stringstream sym;
    while (!eof() && is_symbol_n(currentc())) {
      sym << currentc();
      nextc();
    }
    return sym.str();
  }


  Parser::Args Parser::parse_args() {
    auto args = Args{};
    auto skipped_ws = std::string{};

    while (!eof() && currentc() == '{') {
      nextc();

      std::stringstream arg;
      auto done = false;
      auto bracecount = 0;
      auto brace_srcpos = std::list<std::string>{srcpos()};

      while (!done && !eof()) {
        auto sp = srcpos();
        auto c = currentc();
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

      args.push_back(utils::trim_copy(arg.str()));
      skipped_ws = skip_ws();
    }

    unread(skipped_ws);

    return args;
  }


  Parser::AttrsAndId Parser::args2nl(const Args& args, const TagSpec& tagspec,
                                     size_t lineno_at_start) {
    auto nl = Nodes{};
    auto idstr = estd::optional<std::string>{};

    const auto& attrspecs = tagspec.attrspecs();
    auto attrc = 0u;

    for (const auto& arg : args) {
      // don't reparse argument if ID type
      if (attrspecs[attrc].type() == k_attr_id) {
        if (!idstr) {
          idstr = utils::trim_copy(arg);
        }
      }
      else if (attrspecs[attrc].type() == k_attr_fref) {
        auto abspath = path_rel_abs(_stream->fpath(), utils::trim_copy(arg));
        auto textnd = _grove.make_text_node(abspath.string());
        textnd->set_property(CommonProps::k_attr_name, attrspecs[attrc].name());
        nl.push_back(textnd);
      }
      else {
        auto gb = GroveBuilder(_grove.make_elt_node("<root>"));
        auto p = Parser(_grove, gb, _vars, _catalog, _docspec, _catalog_path, true);
        auto nd = p.parse_stream(
          std::make_shared<Stream>(arg, _stream->fpath(), lineno_at_start));

        auto children = nd->property<Nodes>(CommonProps::k_children);
        if (children.size() == 1u && children[0]->gi() == k_p_tag) {
          auto grandch = children[0]->property<Nodes>(CommonProps::k_children);
          unparent_nodes(grandch);

          for (auto* child : grandch) {
            child->set_property(CommonProps::k_attr_name, attrspecs[attrc].name());
            if (attrspecs[attrc].is_data()) {
              child->set_property(CommonProps::k_data_attr, attrspecs[attrc].name());
            }
            nl.push_back(child);
          }
        }
        else if (children.empty()) {
          auto* txtnd = _grove.make_text_node("");
          txtnd->set_property(CommonProps::k_attr_name, attrspecs[attrc].name());
          nl.push_back(txtnd);
        }
        else {
          unparent_nodes(children);

          for (auto* child : children) {
            child->set_property(CommonProps::k_attr_name, attrspecs[attrc].name());
            if (attrspecs[attrc].is_data()) {
              child->set_property(CommonProps::k_data_attr, attrspecs[attrc].name());
            }
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


  void Parser::parse_at_form() {
    assert(currentc() == '@');

    nextc();

    auto c = currentc();

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


  void Parser::parse_at() {
    *_text << '@';
    nextc();
  }


  void Parser::parse_openbrace() {
    *_text << '{';
    nextc();
  }


  void Parser::parse_closebrace() {
    *_text << '}';
    nextc();
  }


  void Parser::parse_varlookup() {
    nextc();

    auto args = parse_args();
    if (args.size() != 1u) {
      throw ParseException(srcpos(), "Wrong number of args to = expression");
    }

    auto value = _vars[args.front()];
    unread(value);
  }


  void Parser::end_tag(const std::string& endtag) {
    finish_paragraph();
    _grovebuilder.pop_node(endtag);
    skip_ws();

    _is_mixed_content = _is_mixed_content_stack.front();
    _is_mixed_content_stack.pop_front();
  }


  void Parser::parse_endtag() {
    skip_ws();

    if (currentc() == '{') {
      auto args = parse_args();

      if (args.size() == 1) {
        if (args[0] != k_p_tag) {
          end_tag(utils::trim_copy(args[0]));
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


  void Parser::parse_let() {
    auto args = parse_args();
    if (args.size() != 2u) {
      throw ParseException(srcpos(), "Wrong number of args to let expression");
    }
    _vars[args[0]] = args[1];
  }


  fs::path Parser::path_for_include(const fs::path& fpath) {
    if (!fpath.string().empty()) {
      if (fpath.is_absolute())
        return fpath;

      auto currentfp = _stream->fpath();
      return currentfp.parent_path() /= fpath;
    }
    return {};
  }


  void Parser::parse_include() {
    auto sp = srcpos();
    auto args = parse_args();

    if (args.size() != 1u) {
      throw ParseException(srcpos(), "Wrong number of args to include expression");
    }

    try {
      auto inclfp = path_for_include(args.front());
      if (_verbose) {
        std::cerr << "include: " << inclfp << std::endl;
      }

      auto stream = std::make_shared<Stream>(estd::optional<std::string>{}, inclfp);
      push_stream(stream);
    }
    catch (const ParseException& pe) {
      std::cerr << srcpos() << ": Error while parsing include file '" << args.front()
                << "'" << std::endl;
      std::cerr << pe.what() << std::endl;
    }
  }


  void Parser::parse_tag_with_params(const std::string& tag) {
    auto lineno_at_start = lineno();
    auto args = parse_args();

    auto is_root_tag = false;
    if (!_docspec) {
      is_root_tag = true;
      set_docspec_by_root_tag(tag, args);
    }

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
          nd->set_attributes(nl);
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
        nd->set_attributes(nl);
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
        nd->set_attributes(nl);
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


  void Parser::parse_tag() {
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


  void Parser::set_docspec_by_root_tag(const std::string& tag, const Args& args) {
    if (_catalog.find(tag) != _catalog.end()) {
      _docspec = _catalog[tag].get();
      _doctype = tag;
    }
    else {
      auto path = find_model_spec(tag, _catalog_path, ".spec");
      if (!path) {
        throw ParseException("", std::string("Unknown root tag: ") + tag);
      }

      auto docspec = read_model(*path, _catalog_path, _verbose);
      if (docspec.get()) {
        _catalog[tag] = std::move(docspec);
        _docspec = _catalog[tag].get();
        _doctype = tag;
      }
      else {
        throw ParseException("",
                             std::string("Can't read document spec: ") + path->string());
      }

      for (const auto& arg : args) {
        if (arg.find("use=") == 0) {
          for (const auto& mod : utils::split_str(arg.substr(4), ",", true)) {
            if (_verbose) {
              std::cerr << "load extra module " << mod << "\n";
            }

            auto mod_path = find_model_spec(mod, _catalog_path, ".mod");
            if (!mod_path) {
              throw ParseException("", std::string("Unknown module: ") + mod);
            }

            auto mod_docspec = read_model(*mod_path, _catalog_path, _verbose);
            if (mod_docspec.get()) {
              _docspec->merge(mod_docspec.get());
            }
            else {
              throw ParseException("", std::string("Can't read module spec: ") +
                                   mod_path->string());
            }
          }
        }
      }
    }
  }


  void Parser::check_tag_args(const Parser::Args& args, const TagSpec& tagspec) {
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

} // namespace textbook
} // namespace eyestep
