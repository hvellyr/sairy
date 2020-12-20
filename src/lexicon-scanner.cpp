// Copyright (c) 2019 Gregor Klinke
// All rights reserved.

#include "lexicon-scanner.hpp"
#include "estd/memory.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"
#include "nodeutils.hpp"
#include "textbook-parser.hpp"
#include "utils.hpp"

#include "program_options/program_options.hpp"

#include "fspp/filesystem.hpp"

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>


namespace eyestep {

namespace fs = filesystem;

namespace props {
  const std::string k_lx = "lx";
  const std::string k_tag = "tag";
  const std::string k_sub = "sub";
  const std::string k_text = "text";
  const std::string k_app_info = "app-info";
}

constexpr auto lx_tag = "lx";
constexpr auto pd_tag = "pd";
constexpr auto sn_tag = "sn";
constexpr auto se_tag = "se";
constexpr auto hm_tag = "hm";
constexpr auto template_tag = "template";


const NodeClass* lexicon_class_definition() {
  static auto doc_class =
    NodeClass{"lexicon",
              PropertySet{
                {CommonProps::k_source, PropertyType::k_string, false},
                {props::k_app_info, PropertyType::k_string, false},
                {CommonProps::k_children, PropertyType::k_nodelist,
                 true}, // conprop=content
              },
              any_class_definition()};
  return &doc_class;
}


//----------------------------------------------------------------------------------------

namespace lexicon {

  class Template
  {
  public:
    void add_tag(const std::string& tag, const std::string& subtag,
                 const std::string& text) {
      _tags[tag] = std::make_tuple(subtag, text);
    }

    std::unordered_map<std::string, std::tuple<std::string, std::string>> _tags;
  };


  const auto SYMBOL1 = std::unordered_set<char>{
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  };
  const auto SYMBOLn = std::unordered_set<char>{
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
    'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F',
    'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
    'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  };
  const auto WS = std::unordered_set<char>{' ', '\t', '\r', '\n'};


  bool is_symbol1(char c) {
    return SYMBOL1.find(c) != SYMBOL1.end();
  }


  bool is_symbol_n(char c) {
    return SYMBOLn.find(c) != SYMBOLn.end();
  }


  bool is_ws(char c) {
    return WS.find(c) != WS.end();
  }


  bool is_tag_intro(char c) {
    return c == '\\' || c == '.';
  }


  const auto TOPLEVEL_TAGS = std::unordered_set<std::string>{
    "ct", "dt", "hm", "lx", "lc", "ln", "nt", "se", "sn", "st",
  };

  const auto PD_TAGS = std::unordered_set<std::string>{
    "pn", "pnx", "ps",
  };

  const auto TEMPL_TAGS = std::unordered_set<std::string>{
    "ct", "dt", "ln", "st",
  };


  class Parser
  {
    bool _verbose;
    eyestep::Grove& _grove;
    std::shared_ptr<textbook::Stream> _stream;
    Node* _root_nd;
    Node* _current_nd = nullptr;
    Node* _last_pd_nd = nullptr;
    Node* _last_sn_nd = nullptr;
    Node* _last_se_nd = nullptr;
    std::unordered_set<std::string> _current_assigned_tags;
    std::unique_ptr<Template> _current_template;
    std::string _current_tag;
    std::string _current_text;
    std::string _current_lx;
    bool _last_ws = true;
    std::unique_ptr<std::stringstream> _text;

  public:
    Parser(eyestep::Grove& grove, Node* rootnd, bool is_verbose)
      : _verbose(is_verbose)
      , _grove(grove)
      , _root_nd(rootnd)
      , _text(estd::make_unique<std::stringstream>()) {}


    bool eof() {
      return _stream->eof();
    }


    char nextc() {
      return _stream->read();
    }


    char unread(const std::string& buf) {
      return _stream->unread(buf);
    }


    char currentc() {
      return _stream->current_c();
    }


    size_t lineno() {
      return _stream->line_no();
    }


    std::string srcpos() {
      return _stream->srcpos();
    }


    Node* parse_file(const fs::path& fpath) {
      return parse_stream(
        std::make_shared<textbook::Stream>(estd::optional<std::string>{}, fpath));
    }


    Node* parse_string(const std::string& buf, size_t start_line_no) {
      return parse_stream(
        std::make_shared<textbook::Stream>(buf, estd::optional<filesystem::path>{},
                                           start_line_no));
    }


    Node* parse_stream(std::shared_ptr<textbook::Stream> stream) {
      _stream = stream;

      while (!eof()) {
        auto c = currentc();

        if (c == '%') {
          skip_until_eol();
          _last_ws = true;
        }
        else if (is_ws(c)) {
          *_text << c;
          nextc();
          _last_ws = true;
        }
        else if (is_tag_intro(c) && _last_ws) {
          push_text();
          nextc();

          const auto tag = scan_symbol();
          finalize_tag();

          if (tag == lx_tag || tag == template_tag)
            finalize_item(tag);

          _current_tag = tag;
        }
        else {
          *_text << c;
          nextc();
          _last_ws = false;
        }
      }

      push_text();
      finalize_tag();
      finalize_item("");

      return _root_nd;
    }


    void skip_until_eol() {
      while (!eof() && currentc() != '\n') {
        nextc();
      }
      if (currentc() == '\n') {
        nextc();
      }
    }


    std::string scan_symbol() {
      std::stringstream sym;

      while (!eof() && is_symbol_n(currentc())) {
        sym << currentc();
        nextc();
      }

      return sym.str();
    }


    void push_text() {
      _current_text = _text->str();
      _text->str(std::string());
    }


    std::string make_id(const std::string& lx) {
      return utils::replace_str(lx, " ", "_");
    }


    Node* make_tag_nd(const std::string& tag, const std::string& subtag,
                      const std::string& value) {
      auto tag_nd = _grove.make_node(element_class_definition());
      tag_nd->set_property(CommonProps::k_gi, tag);

      if (!subtag.empty())
        tag_nd->add_attribute(props::k_sub, subtag);
      if (!value.empty())
        tag_nd->add_child_node(_grove.make_text_node(value));

      return tag_nd;
    }


    estd::optional<std::tuple<std::string, std::string>> map_tag(const std::string& tag) {
      if (tag.size() > 1) {
        if (tag.find("g") == 0) {
          return std::make_tuple(std::string("gloss"), tag.substr(1));
        }
        else if (tag.find("d") == 0) {
          return std::make_tuple(std::string("def"), tag.substr(1));
        }
        else if (tag.find("x") == 0) {
          return std::make_tuple(std::string("exgloss"), tag.substr(1));
        }
        else if (tag.find("u") == 0) {
          return std::make_tuple(std::string("use"), tag.substr(1));
        }
        else if (tag.find("o") == 0) {
          return std::make_tuple(std::string("only"), tag.substr(1));
        }
        else if (tag.find("lf") == 0) {
          return std::make_tuple(std::string("lexf"), tag.substr(1));
        }
        else if (tag.find("cf") == 0) {
          return std::make_tuple(std::string("cfgloss"), tag.substr(1));
        }
        else if (tag.find("eg") == 0) {
          return std::make_tuple(std::string("etygloss"), tag.substr(1));
        }
      }

      return {};
    }


    void finalize_tag() {
      if (_current_nd) {
        if (_current_tag == lx_tag) {
          auto value = utils::trim_copy(_current_text);
          _current_nd->set_property(CommonProps::k_id, make_id(value));
          _current_nd->set_property(props::k_lx, value);

          _current_lx = value;
        }
        if (_current_tag == hm_tag) {
          _current_nd->set_property(props::k_lx,
                                    _current_lx + "." + utils::trim_copy(_current_text));
        }

        if (_current_tag == lx_tag) {
          auto tag_nd = make_tag_nd(_current_tag, "", utils::trim_copy(_current_text));
          _current_nd->add_child_node(tag_nd);
        }
        else if (_current_tag == se_tag) {
          auto se_nd = make_tag_nd(_current_tag, "", "");
          auto tag_nd = make_tag_nd("lx", "", utils::trim_copy(_current_text));
          se_nd->add_child_node(tag_nd);
          _current_nd->add_child_node(se_nd);
          _last_se_nd = se_nd;
          _last_sn_nd = nullptr;
          _last_pd_nd = nullptr;
        }
        else if (_current_tag == sn_tag) {
          auto tag_nd = make_tag_nd(_current_tag, "", "");
          tag_nd->add_attribute("index", utils::trim_copy(_current_text));
          if (_last_se_nd)
            _last_se_nd->add_child_node(tag_nd);
          else
            _current_nd->add_child_node(tag_nd);

          _last_sn_nd = tag_nd;
          _last_pd_nd = nullptr;
        }
        else {
          for (const auto& val : utils::split_str(_current_text, " ; ", true)) {
            const auto subval = utils::split_str(val, " = ", true);

            std::string tag;
            std::string lang;

            if (auto localized = map_tag(_current_tag)) {
              tag = std::get<0>(*localized);
              lang = std::get<1>(*localized);
            }
            else {
              tag = _current_tag;
            }

            auto tag_nd = subval.size() > 1 ? make_tag_nd(tag, subval[0], subval[1])
                                            : make_tag_nd(tag, "", val);

            if (!lang.empty()) {
              tag_nd->add_attribute("ln", lang);
            }

            if (_last_pd_nd && PD_TAGS.count(tag) > 0) {
              _last_pd_nd->add_child_node(tag_nd);
            }
            else {
              auto is_toplevel = TOPLEVEL_TAGS.count(tag) == 0;
              if (is_toplevel && _last_se_nd)
                _last_se_nd->add_child_node(tag_nd);
              else if (is_toplevel && _last_sn_nd)
                _last_sn_nd->add_child_node(tag_nd);
              else {
                if (TEMPL_TAGS.count(tag) > 0)
                  _current_assigned_tags.insert(tag);

                _current_nd->add_child_node(tag_nd);
              }

              if (tag == pd_tag)
                _last_pd_nd = tag_nd;
            }
          }
        }
      }
      else if (_current_template && _current_tag != template_tag &&
               TEMPL_TAGS.count(_current_tag) > 0) {
        for (const auto& val : utils::split_str(_current_text, " ; ", true)) {
          const auto subval = utils::split_str(val, " = ", true);

          if (subval.size() > 1) {
            _current_template->add_tag(_current_tag, subval[0], subval[1]);
          }
          else
            _current_template->add_tag(_current_tag, "", val);
        }
      }
    }


    void finalize_item(const std::string& tag) {
      if (_current_nd) {
        if (_current_template) {
          for (const auto& t : _current_template->_tags) {
            if (_current_assigned_tags.count(t.first) == 0) {
              _current_nd->add_child_node(
                make_tag_nd(t.first, std::get<0>(t.second), std::get<1>(t.second)));
            }
          }
        }

        _root_nd->add_child_node(_current_nd);
        _current_tag = "";
        _current_text = "";

        _current_nd = nullptr;
        _current_assigned_tags.clear();
        _last_pd_nd = nullptr;
        _last_sn_nd = nullptr;
        _last_se_nd = nullptr;
        _current_lx = {};
      }

      if (tag == lx_tag) {
        _current_nd = _grove.make_node(element_class_definition());
        _current_nd->set_property(CommonProps::k_gi, "lexem");
      }
      else if (tag == template_tag) {
        _current_template = estd::make_unique<Template>();
      }
    }
  };
}


//----------------------------------------------------------------------------------------

LexiconScanner::LexiconScanner() = default;


LexiconScanner::LexiconScanner(const program_options::variables_map& args)
  : _debug(false) {

  if (!args.empty()) {
    _debug = args.count("debug") != 0;
  }
}


program_options::options_description LexiconScanner::program_options() const {
  namespace po = program_options;

  auto opts_title = std::string("Lexicon parser [selector: '") + scanner_id() + "']";
  auto desc = po::options_description(opts_title);
  return desc;
}


Node* LexiconScanner::scan_file(eyestep::Grove& grove, const fs::path& srcfile) {
  auto* doc_node = grove.make_node(lexicon_class_definition());

  doc_node->set_property(CommonProps::k_source, srcfile.string());
  doc_node->set_property(props::k_app_info, "lexicon");

  auto parser = lexicon::Parser(grove, doc_node, false);
  parser.parse_file(srcfile);

  if (_debug) {
    serialize(std::cerr, doc_node);
  }

  return doc_node;
}

} // ns eyestep
