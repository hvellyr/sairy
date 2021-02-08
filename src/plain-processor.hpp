// Copyright (c) 2021 Gregor Klinke
// All rights reserved.

#pragma once

#include "abstract-processor.hpp"

#include "fo.hpp"

#include "cxxopts.hpp"

#include <string>
#include <utility>
#include <vector>


namespace eyestep {

class Sosofo;
class IFormattingObject;
template <typename T>
class IFoProcessor;

namespace plain {
  enum Quadding
  {
    left,
    right,
    center,
    justify
  };


  class LayoutCtx
  {
  public:
    void register_pageno(size_t id, const std::string& value);

    int current_pageno();
    void set_current_pageno(int pn);

    std::unordered_map<size_t, std::string> _pagenos;
    int _pageno = 0;
  };


  class Para
  {
  public:
    Para() = default;
    Para(Quadding quad, fo::LengthSpec first, fo::LengthSpec last,
         fo::LengthSpec space_before, fo::LengthSpec space_after, bool keep_with_prev,
         bool keep_with_next)
      : _quad(quad)
      , _first_line_indent(std::move(first))
      , _last_line_exdent(std::move(last))
      , _space_before(std::move(space_before))
      , _space_after(std::move(space_after))
      , _keep_with_prev(keep_with_prev)
      , _keep_with_next(keep_with_next) {}

    Para(const Para& other) = default;
    Para& operator=(const Para& other) = default;

    Quadding _quad = left;
    fo::LengthSpec _first_line_indent;
    fo::LengthSpec _last_line_exdent;
    fo::LengthSpec _space_before;
    fo::LengthSpec _space_after;
    bool _keep_with_prev = false;
    bool _keep_with_next = false;
  };


  struct Glue
  {
    Glue() = default;
    Glue(double val, double shrink, double grow)
      : _val(val)
      , _shrink(shrink)
      , _grow(grow) {}

    double _val = 0.0;
    double _shrink = 0.0;
    double _grow = 0.0;

    Glue& operator+=(const Glue& rhs) {
      _val += rhs._val;
      _shrink += rhs._shrink;
      _grow += rhs._grow;
      return *this;
    }

    Glue operator+(const Glue& rhs) const {
      auto tmp = *this;
      tmp += rhs;
      return tmp;
    }

    Glue operator+=(const fo::LengthSpec& spec) {
      if (spec._min)
        _shrink += *spec._min;
      if (spec._max)
        _grow += *spec._max;
      _val += spec._value;
      return *this;
    }

    Glue operator+(const fo::LengthSpec& spec) const {
      auto tmp = *this;
      tmp += spec;
      return tmp;
    }
  };


  struct VSpace
  {
    bool _bop = false;
    bool _keep_with_prev = false;
    bool _keep_with_next = false;
    fo::LengthSpec _length;
  };


  class Pageno
  {
  public:
    Pageno() = default;
    Pageno(std::string refid)
      : _id(next_id())
      , _refid(std::move(refid)) {}

    Pageno(const Pageno& rhs)
      : _id(next_id())
      , _refid(rhs._refid)
      , _value(rhs._value) {}

    Pageno& operator=(const Pageno& rhs) = delete;

    Pageno(Pageno&& rhs) = default;
    Pageno& operator=(Pageno&& rhs) = default;

    std::string value() const;
    std::string update_value(LayoutCtx& ctx) const;

    static size_t next_id();

    const size_t _id = 0;
    std::string _refid;
    mutable estd::optional<std::string> _value;
  };


  class Token
  {
  public:
    enum Kind
    {
      none,
      chars,      // box
      ws,         // lengthspec
      glue,       // glue
      bop,        // begin-of-paragraph
      eop,        // end-of-paragraph
      vspace,     // vertical space material
      area_break, // force a break
      pageno,     // produce a pageno (#current = current)
    };

    Token() = default;

    Token(Kind kind)
      : _kind(kind) {}

    Token(Kind kind, const std::string& str)
      : _kind(kind)
      , _str(str) {}

    Token(Kind kind, Pageno no)
      : _kind(kind)
      , _pageno(std::move(no)) {}

    Token(const fo::LengthSpec& length)
      : _kind(ws)
      , _length(length) {}

    Token(const Glue& gl)
      : _kind(glue)
      , _glue(gl) {}

    Token(const Para& para)
      : _kind(bop)
      , _para(para) {}

    Token(const VSpace& space)
      : _kind(vspace)
      , _vspace(space) {}

    ~Token() {
      clear();
    }

    Token(const Token& rhs) {
      *this = rhs;
    }

    void clear() {
      switch (_kind) {
      case none:
      case eop:
      case area_break:
        break;
      case chars:
        _str.~basic_string<char>();
        break;
      case ws:
        _length.~LengthSpec();
        break;
      case glue:
        _glue.~Glue();
        break;
      case bop:
        _para.~Para();
        break;
      case vspace:
        _vspace.~VSpace();
        break;
      case pageno:
        _pageno.~Pageno();
        break;
      }
      _kind = none;
    }

    Token& operator=(const Token& rhs) {
      if (this != &rhs) {
        clear();

        _kind = rhs._kind;

        switch (rhs._kind) {
        case none:
        case eop:
        case area_break:
          break;
        case chars:
          new (&_str) std::string(rhs._str);
          break;
        case ws:
          new (&_length) fo::LengthSpec(rhs._length);
          break;
        case glue:
          new (&_glue) Glue(rhs._glue);
          break;
        case bop:
          new (&_para) Para(rhs._para);
          break;
        case vspace:
          new (&_vspace) VSpace(rhs._vspace);
          break;
        case pageno:
          new (&_pageno) Pageno(rhs._pageno);
          break;
        }
      }

      return *this;
    }

    double length(LayoutCtx& ctx) const;

    Kind _kind = none;
    union
    {
      fo::None _none;
      std::string _str;
      fo::LengthSpec _length;
      Para _para;
      Glue _glue;
      VSpace _vspace;
      Pageno _pageno;
    };
  };


  enum class Mode
  {
    vmode,
    hmode
  };


  struct LineSnapshot
  {
    size_t _tokidx = 0;
    Para _para;
    Mode _mode;
  };

  struct HBox
  {
    double _line_width = 0.0;
    Glue _glue;
    int _glue_slots = 0;
    double _fixed_width = 0.0;
    bool _bop = false;
    bool _keep_with_prev = false;
    bool _keep_with_next = false;
    std::vector<Token> _tokens;
    LineSnapshot _built_from;
  };


  class DispObj
  {
  public:
    enum Kind
    {
      none,
      hbox,
      vspace,
      area_break,
    };

    DispObj(HBox box)
      : _kind(hbox)
      , _box(std::move(box)) {}

    DispObj(Kind kind)
      : _kind(kind) {}

    DispObj(const fo::LengthSpec& length, bool bop, bool keep_with_prev,
            bool keep_with_next)
      : _kind(vspace)
      , _vspace(VSpace{bop, keep_with_prev, keep_with_next, length}) {}

    DispObj(const VSpace& space)
      : _kind(vspace)
      , _vspace(space) {}

    DispObj(const DispObj& rhs) {
      *this = rhs;
    }

    DispObj& operator=(const DispObj& rhs) {
      if (this != &rhs) {
        clear();

        _kind = rhs._kind;

        switch (rhs._kind) {
        case none:
        case area_break:
          break;
        case hbox:
          new (&_box) HBox(rhs._box);
          break;
        case vspace:
          new (&_vspace) VSpace(rhs._vspace);
          break;
        }
      }
      return *this;
    }

    ~DispObj() {
      clear();
    }

    void clear() {
      switch (_kind) {
      case none:
      case area_break:
        break;
      case hbox:
        _box.~HBox();
        break;
      case vspace:
        _vspace.~VSpace();
        break;
      }
      _kind = none;
    }

    std::string data(LayoutCtx& ctx) const;

    double height() const;
    bool is_area_break() const;
    bool is_begin_of_paragraph() const;
    bool is_vspace() const;
    bool is_conditional_vspace() const;
    bool is_infinite_vspace() const;
    bool keep_with_next() const;
    bool keep_with_prev() const;

    Kind _kind = none;
    union
    {
      fo::None _none;
      HBox _box;
      VSpace _vspace;
    };
  };


  enum class AreaKind
  {
    fixed,     // the content is re-generated for each page anew
    continued, // the content is continued onto each page.  Only continued areas
               // will result in area-breaks
  };

  class RegionSpec
  {
  public:
    double _x = 0.0;
    double _y = 0.0;
    double _width = 0.0;
    double _height = 0.0;
    Quadding _quad = Quadding::left;
  };


  class AreaLayoutState
  {
  public:
    size_t _tokidx = 0;
    Mode _mode;
    size_t _begin_of_area = 0;
    Para _para;
    bool _para_done = false;

    size_t _last_page_begin = 0;
  };


  class Area
  {
  public:
    Area(const std::string& port_name, AreaKind kind, RegionSpec spec);

    AreaKind kind() const {
      return _kind;
    }

    const std::string& port_name() const {
      return _port;
    }

    const RegionSpec& spec() const {
      return _spec;
    }

    // in pt
    double line_width() const;
    double area_height() const;
    int widow_count() const;
    int orphan_count() const;

    void put_str(const std::string& str);
    void put_page_number(const std::string& refid);
    void add_hspace(const fo::LengthSpec& space);
    void add_vspace(const fo::LengthSpec& space, bool keep_with_prev,
                    bool keep_with_next);
    void add_break();
    void begin_paragraph(Quadding quad, const fo::LengthSpec& first_line_indent,
                         const fo::LengthSpec& last_line_exdent,
                         const fo::LengthSpec& space_before,
                         const fo::LengthSpec& space_after, bool keep_with_prev,
                         bool keep_with_next);
    void end_paragraph();

    void check_for_area_break(size_t& begin_of_area);
    void layout_vertically(size_t from, size_t to);
    void layout_vertically();

    AreaLayoutState start_layout();
    void layout_one_page(LayoutCtx& ctx, AreaLayoutState& state, bool x);

    //! Indicates whether this area has more material to layout
    bool has_more(AreaLayoutState& state) const;

    //--------

    AreaKind _kind = AreaKind::continued;
    std::string _port;
    RegionSpec _spec;

    std::vector<Token> _tokens;
    std::vector<DispObj> _lines;

    bool _verbatim = false;
    Quadding _quad = Quadding::left;
    fo::LengthSpec _first_line_indent = fo::LengthSpec(fo::kInline, 0.0, fo::k_pt);
    fo::LengthSpec _last_line_exdent = fo::LengthSpec(fo::kInline, 0.0, fo::k_pt);
  };


  class PageSet
  {
  public:
    PageSet(bool verbose)
      : _verbose(verbose) {}

    void layout();

    void ship(LayoutCtx& ctx, const std::vector<DispObj>& output);

    void add_area(Area area);
    Area* area(const char* port_name);

    bool _verbose = false;
    std::vector<Area> _areas;
  };

} // namespace plain


class PlainProcessor : public AbstractProcessor<PlainProcessor>
{
  using Super = AbstractProcessor;

  bool _verbose = false;
  int _line_width = 80;
  plain::Area* _current_area;
  plain::PageSet* _current_pageset = nullptr;
  std::vector<std::shared_ptr<plain::PageSet>> _pagesets;

public:
  PlainProcessor() = default;
  PlainProcessor(const cxxopts::ParseResult& args);

  std::string proc_id() const override {
    return "plain";
  }

  std::string default_output_extension() const override {
    return ".txt";
  }

  void add_program_options(cxxopts::Options& options) const override;

  const IFoProcessor<PlainProcessor>*
  lookup_fo_processor(const std::string& fo_classname) const override;

  void after_rendering() override;

  void set_current_area(plain::Area* area);
  plain::Area* current_area();
  void set_current_pageset(plain::PageSet* pageset);
  plain::PageSet* current_pageset();
  void add_pageset(std::shared_ptr<plain::PageSet> ps);

  bool is_verbose() const {
    return _verbose;
  }
};


namespace plain {
  struct AreaCtx
  {
    PlainProcessor* _po = nullptr;
    plain::Area* _prev_area = nullptr;

    AreaCtx(PlainProcessor* po, plain::Area* area)
      : _po(po)
      , _prev_area(_po->current_area()) {
      _po->set_current_area(area);
    }

    ~AreaCtx() {
      _po->set_current_area(_prev_area);
    }
  };

  struct PageSetCtx
  {
    PlainProcessor* _po = nullptr;
    plain::PageSet* _prev_pageset = nullptr;

    PageSetCtx(PlainProcessor* po, plain::PageSet* pageset)
      : _po(po)
      , _prev_pageset(_po->current_pageset()) {
      _po->set_current_pageset(pageset);
    }

    ~PageSetCtx() {
      _po->set_current_pageset(_prev_pageset);
    }
  };
} // namespace plain
} // namespace eyestep
