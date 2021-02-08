// Copyright (c) 2021 Gregor Klinke
// All rights reserved.

#include "plain-processor.hpp"

#include "estd/memory.hpp"
#include "fo-processor.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"
#include "utils.hpp"

#include "cxxopts.hpp"

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iomanip>
#include <iostream>
#include <limits>
#include <numeric>
#include <random>
#include <sstream>


namespace eyestep {

namespace {
  double max_inf() {
    return std::numeric_limits<double>::infinity();
  }


  fo::LengthSpec null_pt() {
    return fo::LengthSpec(fo::kDimen, 0.0, fo::k_pt);
  }


  fo::LengthSpec null_fill_pt() {
    return fo::LengthSpec(fo::kDimen, 0.0, fo::k_pt, {}, max_inf());
  }


  class PlainNotImplProcessor : public IFoProcessor<PlainProcessor>
  {
  public:
    void render(PlainProcessor* po, const IFormattingObject* fo) const override {
      // Simple passthru for now
      auto& text_port = fo->port("text");
      if (text_port.length() > 0) {
        po->render_sosofo(&text_port);
      }
    }
  };


  class PlainLiteralFoProcessor : public IFoProcessor<PlainProcessor>
  {
  public:
    void render(PlainProcessor* po, const IFormattingObject* fo) const override {
      auto str = static_cast<const fo::Literal*>(fo)->text();

      if (auto area = po->current_area()) {
        area->put_str(str);
      }
    }
  };


  class PlainPageNumberProcessor : public IFoProcessor<PlainProcessor>
  {
  public:
    void render(PlainProcessor* po, const IFormattingObject* fo) const override {
      if (auto area = po->current_area()) {
        auto refid = po->property("refid", std::string("#current"));
        if (!refid.empty())
          area->put_page_number(refid);
      }
    }
  };


  class PlainDisplayGroupFoProcessor : public IFoProcessor<PlainProcessor>
  {
  public:
    void render(PlainProcessor* po, const IFormattingObject* fo) const override {
      auto space_before = po->property("space-before", null_pt());
      auto space_after = po->property("space-after", null_pt());

      auto keep_with_prev = po->property("keep-with-previous?", false);
      auto keep_with_next = po->property("keep-with-next?", false);

      if (auto area = po->current_area()) {
        if (auto break_before = po->property_or_none<std::string>("break-before")) {
          if (*break_before == "page")
            area->add_break();
        }

        area->add_vspace(space_before, keep_with_prev, keep_with_prev);
        po->render_sosofo(&fo->port("text"));
        area->add_vspace(space_after, keep_with_next, keep_with_next);

        if (auto break_after = po->property_or_none<std::string>("break-after")) {
          if (*break_after == "page")
            area->add_break();
        }
      }
    }
  };


  class PlainParagraphFoProcessor : public IFoProcessor<PlainProcessor>
  {
  public:
    void render(PlainProcessor* po, const IFormattingObject* fo) const override {
      auto get_quadding = [&]() {
        auto quadding = po->property("quadding", std::string("left"));
        if (quadding == "left")
          return plain::Quadding::left;
        else if (quadding == "right")
          return plain::Quadding::right;
        else if (quadding == "center")
          return plain::Quadding::center;
        return plain::Quadding::justify;
      };

      auto ind = po->property("first-line-start-indent",
                              fo::LengthSpec(fo::kInline, 0.0, fo::k_pt));

      auto quad = get_quadding();
      auto ext =
        po->property("last-line-end-indent",
                     (quad == plain::Quadding::justify
                        ? fo::LengthSpec(fo::kInline, 10.0, fo::k_pt, 1.0, max_inf())
                        : fo::LengthSpec(fo::kInline, 0.0, fo::k_pt)));

      auto space_before = po->property("space-before", null_pt());
      auto space_after = po->property("space-after", null_pt());

      auto keep_with_prev = po->property("keep-with-previous?", false);
      auto keep_with_next = po->property("keep-with-next?", false);

      if (auto area = po->current_area()) {
        area->begin_paragraph(quad, ind, ext, space_before, space_after, keep_with_prev,
                              keep_with_next);
        po->render_sosofo(&fo->port("text"));
        area->end_paragraph();
      }
    }
  };


  template <typename Functor>
  void render_with_new_area(PlainProcessor* po, plain::PageSet* pageset,
                            const char* port_nm, plain::AreaKind kind,
                            plain::RegionSpec spec, Functor f) {
    pageset->add_area(plain::Area{port_nm, kind, spec});

    plain::PageSetCtx ps_ctx{po, pageset};

    auto area = pageset->area(port_nm);
    plain::AreaCtx area_ctx{po, area};

    f(area);
  }


  void build_header_or_footer(PlainProcessor* po, plain::Area* area,
                              estd::optional<std::shared_ptr<Sosofo>> left,
                              estd::optional<std::shared_ptr<Sosofo>> center,
                              estd::optional<std::shared_ptr<Sosofo>> right) {
    area->begin_paragraph(plain::Quadding::justify,
                          null_pt(), // indent
                          null_pt(), // exdent
                          null_pt(), // space_before
                          null_pt(), // space_after
                          false,     // keep_with_prev,
                          false);    //

    if (left) {
      po->render_sosofo(left->get());
    }
    area->add_hspace(null_fill_pt());
    if (center) {
      po->render_sosofo(center->get());
    }
    area->add_hspace(null_fill_pt());
    if (right) {
      po->render_sosofo(right->get());
    }

    area->end_paragraph();
  }


  class PlainSimplePageProcessorProcessor : public IFoProcessor<PlainProcessor>
  {
  public:
    void render(PlainProcessor* po, const IFormattingObject* fo) const override {
      auto pageset = std::make_shared<plain::PageSet>(po->is_verbose());
      po->add_pageset(pageset);

      auto leftheader = po->property_or_none<std::shared_ptr<Sosofo>>("left-header");
      auto centerheader = po->property_or_none<std::shared_ptr<Sosofo>>("center-header");
      auto rightheader = po->property_or_none<std::shared_ptr<Sosofo>>("right-header");

      auto leftfooter = po->property_or_none<std::shared_ptr<Sosofo>>("left-footer");
      auto centerfooter = po->property_or_none<std::shared_ptr<Sosofo>>("center-footer");
      auto rightfooter = po->property_or_none<std::shared_ptr<Sosofo>>("right-footer");


      render_with_new_area(po, pageset.get(), "header", plain::AreaKind::fixed,
                           plain::RegionSpec{}, //
                           [&](plain::Area* area) {
                             build_header_or_footer(po, area, leftheader, centerheader,
                                                    rightheader);
                           });

      render_with_new_area(po, pageset.get(), "text", plain::AreaKind::continued,
                           plain::RegionSpec{}, //
                           [&](plain::Area* area) {
                             po->render_sosofo(&fo->port("text"));
                           });

      render_with_new_area(po, pageset.get(), "footer", plain::AreaKind::fixed,
                           plain::RegionSpec{}, //
                           [&](plain::Area* area) {
                             build_header_or_footer(po, area, leftfooter, centerfooter,
                                                    rightfooter);
                           });
    }
  };
} // namespace


//------------------------------------------------------------------------------

PlainProcessor::PlainProcessor(const cxxopts::ParseResult& args) {
  _verbose = args.count("verbose") != 0;
  if (args.count("width")) {
    try {
      _line_width = stoi(args["width"].as<std::string>());
    }
    catch (const std::exception&) {
    }
  }
}


void PlainProcessor::add_program_options(cxxopts::Options& options) const {
  options.add_options("Plain renderer")
    // clang-format off
    ("width", "the number of characters per line.  Default is 80",
              cxxopts::value<std::string>())
    ;
  // clang-format on
}


const IFoProcessor<PlainProcessor>*
PlainProcessor::lookup_fo_processor(const std::string& fo_classname) const {
  static auto procs =
    std::map<std::string, std::shared_ptr<IFoProcessor<PlainProcessor>>>{
      {"#literal", std::make_shared<PlainLiteralFoProcessor>()},
      {"#external-graphic", std::make_shared<PlainNotImplProcessor>()},
      {"#paragraph", std::make_shared<PlainParagraphFoProcessor>()},
      {"#paragraph-break", std::make_shared<PlainNotImplProcessor>()},
      {"#display-group", std::make_shared<PlainDisplayGroupFoProcessor>()},
      {"#box", std::make_shared<PlainNotImplProcessor>()},
      {"#simple-page-sequence", std::make_shared<PlainSimplePageProcessorProcessor>()},
      {"#sequence", std::make_shared<PlainNotImplProcessor>()},
      {"#line-field", std::make_shared<PlainNotImplProcessor>()},
      {"#page-number", std::make_shared<PlainPageNumberProcessor>()},
      {"#anchor", std::make_shared<PlainNotImplProcessor>()},
      {"#link", std::make_shared<PlainNotImplProcessor>()},
      {"#score", std::make_shared<PlainNotImplProcessor>()},
      {"#simple-column-set-sequence", std::make_shared<PlainNotImplProcessor>()},
    };

  auto i_find = procs.find(fo_classname);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void PlainProcessor::set_current_area(plain::Area* area) {
  _current_area = area;
}


plain::Area* PlainProcessor::current_area() {
  return _current_area;
}


void PlainProcessor::set_current_pageset(plain::PageSet* pageset) {
  _current_pageset = pageset;
}


plain::PageSet* PlainProcessor::current_pageset() {
  return _current_pageset;
}


void PlainProcessor::add_pageset(std::shared_ptr<plain::PageSet> ps) {
  _pagesets.push_back(std::move(ps));
}


void PlainProcessor::after_rendering() {
  Super::after_rendering();

  for (const auto& ps : _pagesets) {
    ps->layout();
  }
}


//------------------------------------------------------------------------------

namespace plain {
  constexpr auto point_per_char = 7.0;
  constexpr auto point_per_line = 14.0;


  //------------------------------

  void LayoutCtx::register_pageno(size_t id, const std::string& value) {
    auto it = _pagenos.find(id);
    if (it != end(_pagenos))
      it->second = value;
    else
      _pagenos[id] = value;
  }


  int LayoutCtx::current_pageno() {
    return _pageno;
  }


  void LayoutCtx::set_current_pageno(int pn) {
    _pageno = pn;
  }


  //------------------------------

  size_t Pageno::next_id() {
    static size_t count = 0;
    ++count;
    return count;
  }


  std::string Pageno::value() const {
    return _value ? *_value : std::string{};
  }


  std::string Pageno::update_value(LayoutCtx& ctx) const {
    auto pgno = std::to_string(ctx.current_pageno());

    if (!_value || *_value != pgno) {
      _value = pgno;
      ctx.register_pageno(_id, pgno);
    }

    return *_value;
  }


  //------------------------------

  double Token::length(LayoutCtx& ctx) const {
    switch (_kind) {
    case none:
    case bop:
    case eop:
    case vspace:
    case area_break:
      return 0;
    case ws:
    case glue:
      return 0; //_length._value;
    case chars:
      return utils::utf8_length(_str) * point_per_char;
    case pageno:
      return utils::utf8_length(_pageno.update_value(ctx)) * point_per_char;
    }

    return 0;
  }


  //------------------------------

  double DispObj::height() const {
    switch (_kind) {
    case none:
      return 0;
    case hbox:
      return 1 * point_per_line;
    case area_break:
      return 0;
    case vspace:
      return _vspace._length._value;
    }

    return 0;
  }


  std::string DispObj::data(LayoutCtx& ctx) const {
    if (_kind == hbox) {
      std::stringstream result;

      for (const auto& tok : _box._tokens) {
        if (tok._kind == Token::chars) {
          result << tok._str;
        }
        else if (tok._kind == Token::glue) {
          result << " ";
        }
        else if (tok._kind == Token::pageno) {
          result << tok._pageno.update_value(ctx);
        }
      }

      return result.str();
    }
    return {};
  }


  bool DispObj::is_area_break() const {
    return _kind == area_break;
  }


  bool DispObj::is_begin_of_paragraph() const {
    switch (_kind) {
    case hbox:
      return _box._bop;
    case vspace:
      return _vspace._bop;
    case none:
    case area_break:
      return false;
    }

    return false;
  }


  bool DispObj::is_conditional_vspace() const {
    return _kind == vspace && _vspace._length._conditionalp;
  }


  bool DispObj::is_vspace() const {
    return _kind == vspace;
  }


  bool DispObj::is_infinite_vspace() const {
    return _kind == vspace && _vspace._length._max && *_vspace._length._max == max_inf();
  }


  bool DispObj::keep_with_next() const {
    switch (_kind) {
    case hbox:
      return _box._keep_with_next;
    case vspace:
      return _vspace._keep_with_next;
    case none:
    case area_break:
      return false;
    }

    return false;
  }


  bool DispObj::keep_with_prev() const {
    switch (_kind) {
    case hbox:
      return _box._keep_with_prev;
    case vspace:
      return _vspace._keep_with_prev;
    case none:
    case area_break:
      return false;
    }

    return false;
  }


  //------------------------------

  Area::Area(const std::string& port_name, AreaKind kind, RegionSpec spec)
    : _kind(kind)
    , _port(port_name)
    , _spec(spec) {}


  double Area::line_width() const {
    return 72 * point_per_char;
  }


  double Area::area_height() const {
    return 50 * point_per_line;
  }


  void Area::add_hspace(const fo::LengthSpec& space) {
    _tokens.push_back(space);
  }


  void Area::add_vspace(const fo::LengthSpec& length, bool keep_with_prev,
                        bool keep_with_next) {
    _tokens.push_back(VSpace{false, keep_with_prev, keep_with_next, length});
  }


  void Area::add_break() {
    _tokens.push_back(Token{Token::area_break});
  }


  void Area::begin_paragraph(Quadding quad, const fo::LengthSpec& first_line_indent,
                             const fo::LengthSpec& last_line_exdent,
                             const fo::LengthSpec& space_before,
                             const fo::LengthSpec& space_after, bool keep_with_prev,
                             bool keep_with_next) {
    _tokens.push_back(Token{Para{quad, first_line_indent, last_line_exdent, space_before,
                                 space_after, keep_with_prev, keep_with_next}});
  }


  void Area::end_paragraph() {
    _tokens.push_back(Token{Token::eop});
  }


  void Area::put_str(const std::string& str) {
    size_t pos = 0;

    auto last_char_was_comma = false;
    while (pos < str.size()) {
      auto c = str[pos];

      if (c == ' ' || c == '\n' || c == '\r' || c == '\t') {
        auto fpos = str.find_first_not_of(" \n\r\t", pos);
        if (fpos != std::string::npos) {
          if (_verbatim)
            _tokens.push_back(Token{Token::chars, std::string(fpos - pos, ' ')});
          else if (last_char_was_comma)
            _tokens.push_back(
              Token{fo::LengthSpec{fo::kDimen, 2 * point_per_char, fo::k_pt, 4, 4}});
          else
            _tokens.push_back(
              Token{fo::LengthSpec{fo::kDimen, point_per_char, fo::k_pt, 4, 4}});
          pos = fpos;
        }
        else {
          if (_verbatim)
            _tokens.push_back(Token{Token::chars, std::string(str.size() - pos, ' ')});
          else if (last_char_was_comma)
            _tokens.push_back(
              Token{fo::LengthSpec{fo::kDimen, 2 * point_per_char, fo::k_pt, 4, 4}});
          else
            _tokens.push_back(
              Token{fo::LengthSpec{fo::kDimen, point_per_char, fo::k_pt, 4, 4}});
          break;
        }
        last_char_was_comma = false;
      }
      else {
        auto fpos = str.find_first_of(",.;/-)]+*: \n\r\t", pos);
        if (fpos != std::string::npos) {
          if (str[fpos] == ' ' || str[fpos] == '\n' || str[fpos] == '\r' ||
              str[fpos] == '\t') {
            _tokens.push_back(Token{Token::chars, str.substr(pos, fpos - pos)});
            pos = fpos;
          }
          else {
            _tokens.push_back(Token{Token::chars, str.substr(pos, fpos - pos + 1)});
            pos = fpos + 1;
          }

          last_char_was_comma =
            (str[fpos] == ',' || str[fpos] == ';' || str[fpos] == '.');
        }
        else {
          last_char_was_comma = false;
          _tokens.push_back(Token{Token::chars, str.substr(pos)});
          break;
        }
      }
    }
  }


  void Area::put_page_number(const std::string& refid) {
    _tokens.push_back(Token{Token::pageno, Pageno{refid}});
  }


  namespace {
    size_t number_of_inf_glue(const HBox& ln) {
      return accumulate(begin(ln._tokens), end(ln._tokens), 0u,
                        [](size_t sum, const Token& tok) {
                          return (tok._kind == Token::glue &&
                                  tok._glue._grow == max_inf())
                                   ? sum + 1
                                   : sum;
                        });
    }
  } // namespace


  // next steps:
  // x ship into pages
  // x display spaces
  // x breaks
  // x ignore conditional? display spaces at area/page start
  // x implement widow and orphan count
  // x keep together
  //
  // - header and footer for simple-page-sequence
  //
  // - count page numbers
  //
  // - render #page-number FO, with putting it as placeholder with best guess
  //   length into token stream; cache looked up data (e.g. the current page
  //   number) per anchor; record all indirect FO, with their rendered value
  //   resolution; after the layout() run, check whether any indirect FO would
  //   produce a different result; if so, rerun layout, without resetting cached
  //   values; do this n times and complain if the result doesn't stabilize.
  //
  // - start_indent
  // - end_indent
  //
  // - try multi-area per pageset
  //
  // - define last-line-quadding: (with "relative" meaning "start" when
  //   quadding: is "start".  Or "start", "end", "center", or "justify").
  //   default last-line-end-indent to 0pt, and produce the inf glue
  //   automatically.

  int Area::widow_count() const {
    return 2;
  }


  int Area::orphan_count() const {
    return 2;
  }


  void Area::check_for_area_break(size_t& begin_of_area) {
    auto running_height = 0.0;
    auto last_bop_idx = -1;

    auto find_end_of_para = [&](size_t start_idx) {
      for (auto i = start_idx; i < _lines.size(); ++i) {
        if (_lines[i].is_begin_of_paragraph() && i > start_idx) {
          return i - 1;
        }
      }

      return _lines.size();
    };

    auto insert_area_break = [&](size_t at_idx) {
      _lines.insert(next(begin(_lines), at_idx), DispObj{DispObj::area_break});
      begin_of_area = at_idx + 1;
      running_height = 0.0;
    };

    auto count_nonspace_lines = [&](size_t from, size_t to) -> size_t {
      auto count = size_t(0);
      for (auto i = from; i < to; ++i) {
        if (_lines[i]._kind == DispObj::hbox) {
          ++count;
        }
      }
      return count;
    };

    auto count_para_lines = [&](size_t from) -> size_t {
      for (auto i = int(from); i >= 0; --i) {
        if (_lines[i].is_begin_of_paragraph()) {
          return find_end_of_para(from) - i;
        }
      }

      return 0;
    };

    auto break_allowed_at = [&](size_t idx) {
      if (_lines[idx].keep_with_prev())
        return false;

      if (idx > 0) {
        if (_lines[idx - 1].keep_with_next())
          return false;
      }

      return true;
    };

    auto find_possible_leading_vspace = [&](size_t from) {
      if (from > 0) {
        auto space_idx = from;
        for (auto i = int(from - 1); i >= 0; --i) {
          if (!_lines[i].is_vspace())
            return space_idx;
          space_idx = size_t(i);
        }
      }
      return from;
    };

    auto find_allowed_prev_break = [&](size_t from) -> int {
      for (auto i = int(from); i >= 0; --i) {
        if (break_allowed_at(i) &&
            (count_nonspace_lines(i, find_end_of_para(i)) >= orphan_count() ||
             count_para_lines(i) < orphan_count())) {
          return find_possible_leading_vspace(i);
        }
      }

      return -1;
    };

    auto lnidx = begin_of_area;
    while (lnidx < _lines.size()) {
      const auto& ln = _lines[lnidx];

      if (ln.is_area_break()) {
        ++lnidx;
        begin_of_area = lnidx;
        running_height = 0.0;
        continue;
      }

      if (ln.is_conditional_vspace()) {
        if (begin_of_area == lnidx) {
          // conditional vertical space at the area begin is skipped
          _lines.erase(next(begin(_lines), lnidx));
          continue;
        }
      }

      if (ln.is_begin_of_paragraph()) {
        last_bop_idx = lnidx;
      }

      auto display_object_height = ln.height();

      if (running_height + display_object_height < area_height()) {
        running_height += display_object_height;
        ++lnidx;
      }
      else {
        if (count_nonspace_lines(last_bop_idx, lnidx) < widow_count()) {
          // try to reduce glue on the page sofar

          // else: break at the last_bop_idx
          if (break_allowed_at(last_bop_idx)) {
            insert_area_break(last_bop_idx);
            return;
          }
          else {
            auto prev_break_idx = find_allowed_prev_break(last_bop_idx);
            if (prev_break_idx >= 0) {
              insert_area_break(prev_break_idx);
              return;
            }
            else {
              // ok, there's no good break point.  Break at the original position anyway.
              insert_area_break(last_bop_idx);
              return;
            }
          }
        }
        else {
          auto eop_idx = find_end_of_para(lnidx);
          if (count_nonspace_lines(lnidx, eop_idx) < orphan_count()) {
            // find a better previous place to break to avoid orphans
            auto last_possible_break_idx = eop_idx - orphan_count();
            if (count_nonspace_lines(last_bop_idx, last_possible_break_idx) <
                widow_count()) {
              // if not possible break the entire para at last_bop_idx
              // TODO: check whether breaks is allowed here:
              insert_area_break(last_bop_idx);
              return;
            }
            else {
              // TODO: check whether breaks is allowed here:
              insert_area_break(last_possible_break_idx);
              return;
            }
          }
          else {
            // TODO: check whether breaks is allowed here:
            insert_area_break(lnidx);
            return;
          }
        }
      }
    }
  }


  void Area::layout_vertically(size_t from, size_t to) {
    auto total_height = [&]() {
      auto height = 0.0;
      for (auto i = from; i < to; ++i) {
        height += _lines[i].height();
      }
      return height;
    }();
    auto vspace_to_spread = std::max(area_height() - total_height, 0.0);

    if (vspace_to_spread > 0.0) {
      auto inf_vspace_count = [&]() {
        auto count = 0;
        for (auto i = from; i < to; ++i) {
          if (_lines[i].is_infinite_vspace())
            ++count;
        }
        return count;
      }();

      if (inf_vspace_count > 0) {
        auto ext = vspace_to_spread / inf_vspace_count;
        for (auto i = from; i < to; ++i) {
          if (_lines[i].is_infinite_vspace()) {
            _lines[i]._vspace._length._value += ext;
          }
        }
      }
      else {
        auto remain_dyn_spaces = [&]() {
          auto count = 0;
          for (auto i = from; i < to; ++i) {
            if (_lines[i].is_vspace() && _lines[i]._vspace._length._max) {
              ++count;
            }
          }
          return count;
        }();

        auto remain_vspace = vspace_to_spread;

        for (auto i = from; i < to; ++i) {
          if (_lines[i].is_vspace() && _lines[i]._vspace._length._max) {
            auto ext = std::min(remain_vspace / remain_dyn_spaces,
                                *_lines[i]._vspace._length._max);
            _lines[i]._vspace._length =
              fo::LengthSpec(fo::kDisplay, _lines[i]._vspace._length._value + ext,
                             fo::k_pt, 0.0, 0.0, _lines[i]._vspace._length._conditionalp);
            remain_vspace -= ext;
            --remain_dyn_spaces;
          }
        }

        if (remain_vspace > 0.0) {
          // todo; this simply adds the remaining space to the bottom of the page
        }
      }
    }
  }


  void Area::layout_vertically() {
    auto begin_of_area = 0;
    auto idx = 0;
    for (const auto& ln : _lines) {
      if (ln.is_area_break()) {
        layout_vertically(begin_of_area, idx);
        begin_of_area = idx + 1;
      }
      ++idx;
    }
  }


  namespace {
    void start_line(std::vector<Token>& line, Quadding quad) {
      switch (quad) {
      case Quadding::right:
      case Quadding::center:
        line.push_back(Token{Glue{0.0, 0.0, max_inf()}});
      case Quadding::left:
      case Quadding::justify:
        break;
      }
    }


    void end_line(std::vector<Token>& line, Quadding quad) {
      switch (quad) {
      case Quadding::left:
      case Quadding::center:
        line.push_back(Token{Glue{0.0, 0.0, max_inf()}});
        break;
      case Quadding::right:
      case Quadding::justify:
        break;
      }
    }
  } // namespace


  // until end_of_paragraph {
  //   try each in turn: k_normal_width, k_narrow_width, k_normal_width+hyphenation,
  //   k_narrow_width+hyph:
  //     ptr = remember_point
  //     line = collect_line(k_normal, line_width(vertical_pos));
  //     if badness-is-ok(line)
  //       ship_line
  //       continue
  //     else
  //       back to ptr
  //   if none is good:
  //     except the least worst;
  // }

  void Area::layout_one_page(LayoutCtx& ctx, AreaLayoutState& state, bool x) {
    while (state._tokidx < _tokens.size()) {
      auto length_sofar = 0.0;
      auto glue = Glue{};
      auto delayed_glue = Glue{};
      auto glue_slots = 0;
      auto has_delayed_glue_slot = false;

      auto is_begin_of_para = false;

      std::vector<Token> line;
      estd::optional<LineSnapshot> lnsn;

      auto line_done = false;
      auto has_seen_box = false;

      switch (state._mode) {
      case Mode::vmode:
        switch (_tokens[state._tokidx]._kind) {
        case Token::vspace:
          if (!_tokens[state._tokidx]._vspace._length.is_null())
            _lines.push_back(DispObj{_tokens[state._tokidx]._vspace});
          ++state._tokidx;
          break;

        case Token::bop:
          state._mode = Mode::hmode;
          continue;

        case Token::area_break:
          _lines.push_back(DispObj{DispObj::area_break});
          ++state._tokidx;
          break;

        case Token::chars:
        case Token::pageno:
        case Token::none:
        case Token::eop:
        case Token::ws:
        case Token::glue:
          assert(false);
          break;
        }
        break;

      case Mode::hmode:
        while (!line_done) {
          switch (_tokens[state._tokidx]._kind) {
          case Token::bop:
            assert(line.empty());
            state._para = _tokens[state._tokidx]._para;
            start_line(line, state._para._quad);

            lnsn = {LineSnapshot{state._tokidx, state._para, state._mode}};

            is_begin_of_para = true;

            if (!state._para._first_line_indent.is_null()) {
              line.push_back(Token{Glue{} + state._para._first_line_indent});
              glue += state._para._first_line_indent;
            }
            ++state._tokidx;
            break;

          case Token::eop:
            switch (state._para._quad) {
            case Quadding::justify:
              if (!state._para._last_line_exdent.is_null()) {
                line.push_back(Token{Glue{} + state._para._last_line_exdent});
                glue += state._para._last_line_exdent;
              }
              break;
            case Quadding::center:
            case Quadding::right:
            case Quadding::left:
              end_line(line, state._para._quad);
              break;
            }
            ++state._tokidx;
            line_done = true;
            state._para_done = true;
            state._mode = Mode::vmode;
            break;

          case Token::pageno:
          case Token::chars:
            if (!lnsn) {
              lnsn = {LineSnapshot{state._tokidx, state._para, state._mode}};
            }

            if (!has_seen_box && line.empty()) {
              start_line(line, state._para._quad);
            }

            {
              auto toklen = _tokens[state._tokidx].length(ctx);
              if (length_sofar + glue._val + toklen + delayed_glue._val <= line_width()) {
                if (has_delayed_glue_slot) {
                  has_delayed_glue_slot = false;
                  glue_slots++;
                  line.push_back(Token{delayed_glue});
                }

                glue += delayed_glue;
                delayed_glue = {};

                length_sofar += toklen;

                line.push_back(_tokens[state._tokidx]);

                ++state._tokidx;

                has_seen_box = true;
              }
              else {
                end_line(line, state._para._quad);
                line_done = true;
              }
            }
            break;
          case Token::ws:
            if (has_seen_box) {
              if (_tokens[state._tokidx]._length._min ||
                  _tokens[state._tokidx]._length._max)
                has_delayed_glue_slot = true;

              delayed_glue += _tokens[state._tokidx]._length;
            }
            ++state._tokidx;
            break;

          case Token::none:
          case Token::area_break:
          case Token::vspace:
          case Token::glue:
            assert(false);
            break;
          }
        }

        // check badness and retry with different width settings.

        auto needs_bop_marker = is_begin_of_para;

        if (is_begin_of_para) {
          if (!state._para._space_before.is_null()) {
            _lines.push_back(DispObj{state._para._space_before, true,
                                     state._para._keep_with_prev, false});
            needs_bop_marker = false;
          }
        }

        auto disp_obj = DispObj(
          HBox{line_width(), glue, glue_slots, length_sofar, needs_bop_marker,
               is_begin_of_para ? state._para._keep_with_prev : false,
               state._para_done ? state._para._keep_with_next : false, line, *lnsn});
        _lines.push_back(disp_obj);

        lnsn.reset();

        is_begin_of_para = false;

        if (state._para_done) {
          if (!state._para._space_after.is_null()) {
            _lines.push_back(DispObj{state._para._space_after, false, false,
                                     state._para._keep_with_next});
          }

          state._para_done = false;
        }

        break;
      };

      if (state._mode == Mode::vmode) {
        state._last_page_begin = state._begin_of_area;
        check_for_area_break(state._begin_of_area);

        if (state._last_page_begin < state._begin_of_area) {
          layout_vertically(state._last_page_begin, state._begin_of_area);

          // if there's laid out content for the next page, make sure to
          // re-layout it.  It might contain indirect data, which isn't correct
          // on the next page anymore.  E.g. it might contain a PageNo object,
          // which might grow in length by the page break.
          if (state._begin_of_area + 1 < _lines.size()) {
            for (auto l = state._begin_of_area + 1; l < _lines.size(); ++l) {
              if (_lines[l]._kind == DispObj::hbox) {
                auto snsh = _lines[l]._box._built_from;

                _lines.erase(next(begin(_lines), l), end(_lines));
                state._tokidx = snsh._tokidx;
                state._para = snsh._para;
                state._mode = snsh._mode;
                break;
              }
            }
          }

          return;
        }
      }
    }

    if (state._begin_of_area < _lines.size()) {
      state._last_page_begin = state._begin_of_area;
      layout_vertically(state._begin_of_area, _lines.size());

      state._begin_of_area = _lines.size();
    }
  }


  AreaLayoutState Area::start_layout() {
    _lines.clear();

    return AreaLayoutState{0, Mode::vmode, _lines.size(), Para{}, false, 0};
  }


  bool Area::has_more(AreaLayoutState& state) const {
    return state._tokidx < _tokens.size() || state._begin_of_area < _lines.size();
  }


  //------------------------------

  namespace {
    // there's another space to be put somewhere; if there's an infinite space,
    // choose the right most; if not let's put per somewhere random in the line.
    void add_extra_space(const std::vector<Token>& tokens, bool have_inf_glue_slots,
                         std::vector<size_t>& gaps) {
      if (gaps.empty())
        return;

      static std::random_device rd;
      static std::mt19937 randgen(rd());

      if (have_inf_glue_slots) {
        auto gapidx = int(gaps.size()) - 1;
        for (auto it = end(tokens); it != begin(tokens); --it) {
          if (it->_kind == Token::glue) {
            if (it->_glue._grow == max_inf()) {
              if (gapidx > 0) {
                gaps[size_t(gapidx)] += 1;
              }
            }
            --gapidx;
          }
        }
      }
      else {
        auto first = (tokens.front()._kind == Token::glue) ? 1 : 0;
        if (first <= gaps.size() - 1) {
          std::uniform_int_distribution<> distrib(first, gaps.size() - 1);
          auto idx = distrib(randgen);
          gaps[idx] += 1;
        }
      }
    }


    void ship_hbox(LayoutCtx& ctx, const HBox& ln) {
      auto fixed_space = ln._fixed_width + ln._glue._val;
      auto glue_space = std::max(ln._line_width - fixed_space, 0.0);
      auto left_space = 0.0;
      auto right_space = 0.0;

      auto spaces = std::vector<double>{};
      auto inf_glue_slots = number_of_inf_glue(ln);

      auto remain_glue = inf_glue_slots > 0 ? 0 : glue_space;
      auto remain_slots = ln._glue_slots;
      for (const auto& tok : ln._tokens) {
        if (tok._kind == Token::glue) {
          if (inf_glue_slots > 0) {
            if (tok._glue._grow == max_inf()) {
              spaces.push_back(tok._glue._val + glue_space / inf_glue_slots);
            }
            else {
              spaces.push_back(tok._glue._val);
            }
          }
          else if (remain_slots > 0) {
            auto ext = std::min(remain_glue / remain_slots, tok._glue._grow);
            spaces.push_back(tok._glue._val + ext);
            remain_glue -= ext;
            --remain_slots;
          }
          else {
            spaces.push_back(tok._glue._val);
          }
        }
      }
      if (remain_glue > 0) {
        if (ln._glue_slots > 0) {
          size_t i = 0;
          for (const auto& tok : ln._tokens) {
            if (tok._kind == Token::glue) {
              if (tok._glue._grow > 0) {
                spaces[i] += remain_glue / ln._glue_slots;
              }
              ++i;
            }
          }
          remain_glue = 0.0;
        }
      }
      if (remain_glue > 0) {
        right_space = remain_glue;
      }


      auto gaps = std::vector<size_t>{};
      gaps.reserve(spaces.size());

      auto error = 0.0;
      for (auto& gap : spaces) {
        auto correction_space = 0;
        if (error >= point_per_char) {
          correction_space = size_t(error / point_per_char);
          error -= correction_space * point_per_char;
        }

        auto gap_in_chars = gap / point_per_char;

        gaps.push_back(size_t(gap_in_chars + correction_space));

        error += (gap - size_t(gap_in_chars) * point_per_char);
      }
      // compare with epsilon.  The math above might not result in 0.0
      if (error > 0.1) {
        add_extra_space(ln._tokens, inf_glue_slots > 0, gaps);
      }

      // std::cout << "| " << left_space << " |{";
      // for (auto gp : gaps) {
      //   std::cout << gp << " ";
      // }
      // std::cout << "} | " << right_space << "\n";

      if (left_space > 0.0) {
        std::cout << std::string(size_t(left_space / point_per_char), ' ');
      }

      auto end_it = end(ln._tokens);
      for (; end_it != begin(ln._tokens); --end_it) {
        if (prev(end_it)->_kind != Token::glue) {
          break;
        }
      }

      auto gapidx = 0u;
      for (auto it = begin(ln._tokens); it != end_it; ++it) {
        const auto& tok = *it;

        switch (tok._kind) {
        case Token::bop:
        case Token::eop:
        case Token::none:
        case Token::ws:
        case Token::vspace:
        case Token::area_break:
          assert(false);
          break;
        case Token::chars:
          std::cout << tok._str;
          break;
        case Token::pageno: {
          std::cout << tok._pageno.value();
          break;
        }
        case Token::glue:
          if (gapidx < gaps.size()) {
            std::cout << std::string(gaps[gapidx], ' ');
          }
          ++gapidx;
          break;
        }
      }

      // not need to output the right_space

      std::cout << "\n";
    }
  } // namespace


  void PageSet::ship(LayoutCtx& ctx, const std::vector<DispObj>& output) {
    auto lns_per_page = 0;
    auto idx = 0;

    auto ship_lno = [&](const auto& dspobj, bool posbop, const char* sep) {
      if (dspobj.is_begin_of_paragraph() && posbop)
        std::cout << std::setw(4) << idx << ">" << sep;
      else
        std::cout << std::setw(4) << idx << "|" << sep;
    };

    for (const auto& ln : output) {
      switch (ln._kind) {
      case DispObj::none:
        break;
      case DispObj::hbox:
        if (_verbose)
          ship_lno(ln, true, " ");

        ship_hbox(ctx, ln._box);
        ++lns_per_page;
        ++idx;
        break;
      case DispObj::area_break:
        if (_verbose)
          std::cout << "\n[" << lns_per_page << "] ---\n\n";
        else
          std::cout << "\n---\n\n";

        lns_per_page = 0;
        break;
      case DispObj::vspace:
        for (auto i = 0u; i < size_t(round(ln.height() / point_per_line)); ++i) {
          if (_verbose)
            ship_lno(ln, i == 0u, "\n");
          else
            std::cout << "\n";
          ++idx;
        }
        break;
      }
    }
  }


  void PageSet::add_area(Area area) {
    _areas.push_back(std::move(area));
  }


  Area* PageSet::area(const char* port_name) {
    auto it = find_if(begin(_areas), end(_areas),
                      [&](auto& area) { return area.port_name() == port_name; });
    return it != end(_areas) ? &(*it) : nullptr;
  }


  void PageSet::layout() {
    auto ctx = LayoutCtx{};

    auto snapshots = std::vector<AreaLayoutState>(_areas.size());
    for (auto& area : _areas) {
      snapshots.push_back(area.start_layout());
    }

    auto pageno = 1;
    auto output = std::vector<DispObj>{};
    auto has_more = true;

    while (has_more) {
      ctx.set_current_pageno(pageno);
      has_more = false;

      for (auto i = 0u; i < _areas.size(); ++i) {
        auto& area = _areas[i];
        auto& area_state = snapshots[i];

        switch (area.kind()) {
        case AreaKind::continued:
          if (area.has_more(area_state)) {
            area.layout_one_page(ctx, area_state, false);
            has_more |= area.has_more(area_state);
          }
          break;
        case AreaKind::fixed:
          area_state = area.start_layout();
          area.layout_one_page(ctx, area_state, true);
          break;
        }
      }

      // now we have enough material for one page to ship.
      for (auto i = 0u; i < _areas.size(); ++i) {
        auto& area = _areas[i];
        auto& area_state = snapshots[i];

        switch (area.kind()) {
        case AreaKind::continued:
          std::for_each(next(begin(area._lines), area_state._last_page_begin),
                        next(begin(area._lines), area_state._begin_of_area),
                        [&](const auto& dso) {
                          if (dso._kind != DispObj::area_break) {
                            output.push_back(dso);
                          }
                        });
          break;
        case AreaKind::fixed:
          output.insert(end(output),
                        next(begin(area._lines), area_state._last_page_begin),
                        next(begin(area._lines), area_state._begin_of_area));
          break;
        }
      }

      output.push_back(DispObj::area_break);


      ++pageno;
    }

    // TODO: check whether any indirect FO would produce a different value now
    // that all pages are produced.  Relayout everything, but don't reset the
    // cache of reference data.  Do this 2-3 times, that complain and go on.

    ship(ctx, output);
  }

} // namespace plain

} // namespace eyestep
