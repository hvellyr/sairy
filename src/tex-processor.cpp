// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "tex-processor.hpp"

#include "estd/memory.hpp"
#include "fo-processor.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"
#include "utils.hpp"

#include "cxxopts.hpp"
#include "fspp/filesystem.hpp"

#include <cassert>
#include <iostream>
#include <map>
#include <memory>
#include <regex>
#include <sstream>
#include <string>
#include <unordered_set>

namespace eyestep {

namespace fs = filesystem;


//------------------------------------------------------------------------------

namespace {
  const auto k_pt = std::string("pt");
  const auto k_px = std::string("px");
  const auto k_em = std::string("em");
  const auto k_m = std::string("m");
  const auto k_mm = std::string("mm");
  const auto k_cm = std::string("cm");
  const auto k_in = std::string("in");


  std::string unit_name(fo::Unit un) {
    switch (un) {
    case fo::k_pt:
      return k_pt;
    case fo::k_px:
      return k_px;
    case fo::k_m:
      return k_m;
    case fo::k_mm:
      return k_mm;
    case fo::k_cm:
      return k_cm;
    case fo::k_em:
      return k_em;
    case fo::k_in:
      return k_in;
    }
  }


  fo::Unit unit_str2unit(const std::string& str) {
    if (str == k_pt)
      return fo::k_pt;
    else if (str == k_m)
      return fo::k_m;
    else if (str == k_mm)
      return fo::k_mm;
    else if (str == k_cm)
      return fo::k_cm;
    else if (str == k_in)
      return fo::k_in;

    return fo::k_pt;
  }


  std::string dimen2str(const fo::LengthSpec& ls) {
    auto max_inf = std::numeric_limits<double>::infinity();

    std::stringstream ss;
    ss << ls._value << unit_name(ls._unit);

    if (ls._max == max_inf) {
      if (ls._priority > 1)
        ss << " plus 1fill";
      else
        ss << " plus 1fil";
    }
    else if (ls._max != ls._value)
      ss << " plus " << ls._max << unit_name(ls._unit);

    if (ls._min != ls._value)
      ss << " minus " << ls._min << unit_name(ls._unit);

    return ss.str();
  }


  std::string
  crop_paper_size(const std::tuple<fo::LengthSpec, fo::LengthSpec, std::string>& dimen) {
    if (std::get<2>(dimen) == "user") {
      std::stringstream ss;
      ss << "width=" << dimen2str(std::get<0>(dimen))
         << ",height=" << dimen2str(std::get<1>(dimen));
      return ss.str();
    }

    return std::get<2>(dimen);
  }


  std::string exact_dimen2str(const fo::LengthSpec& ls) {
    std::stringstream ss;
    ss << ls._value << unit_name(ls._unit);
    return ss.str();
  }


  std::string max_dimen2str(const fo::LengthSpec& ls) {
    auto max_inf = std::numeric_limits<double>::infinity();

    std::stringstream ss;
    if (ls._max == max_inf)
      ss << "\\hfill{}";
    else
      ss << ls._max << unit_name(ls._unit);
    return ss.str();
  }


  void escape_str_to_stream(TexProcessor* po, std::ostream& os, const std::string& str,
                            tex_detail::TexStyleContext& ctx) {
    auto str32 = utils::utf8_to_u32string(str);

    size_t cidx = 0;
    while (cidx < str32.size()) {
      size_t next_idx = cidx + 1;
      std::stringstream post_op;

      while (next_idx < str32.size() && str32[next_idx] >= 0x0303 &&
             str32[next_idx] <= 0x036f) {
        switch (str32[next_idx]) {
        case 0x0300:
          os << "\\`";
          break;
        case 0x0301:
          os << "\\'";
          break;
        case 0x0302:
          os << "\\^";
          break;
        case 0x0303:
          os << "\\~{";
          post_op << "}";
          break;
        case 0x0308:
          os << "\\\"";
          break;
        case 0x0309: // ring above
          os << "\\r{";
          post_op << "}";
          break;
        }
        ++next_idx;
      }

      auto c = str32[cidx];
      switch (c) {
      case '\\':
        os << "\\textbackslash{}";
        break;
      case '{':
        os << "\\textbraceleft{}";
        break;
      case '}':
        os << "\\textbraceright{}";
        break;
      case '%':
        os << "\\%";
        break;
      case '&':
        os << "\\&";
        break;
      case '<':
        os << "\\tbless{}";
        break;
      case '>':
        os << "\\tbgreater{}";
        break;
      case '$':
        os << "\\textdollar{}";
        break;
      case '_':
        os << "\\textunderscore{}";
        break;
      case '#':
        os << "\\#";
        break;
      case '^':
        os << "\\textasciicircum{}";
        break;
      case '~':
        os << "\\textasciitilde{}";
        break;
      case 0x2e1b:
        os << "\\r{\\textasciitilde{}}";
        break;
      case '|':
        os << "{\\ttfamily|}";
        break;
      case 0x1e60:
        os << "\\.{S}";
        break;
      case 0x1e61:
        os << "\\.{s}";
        break;
      case 0x1e62:
        os << "\\d{S}";
        break;
      case 0x1e63:
        os << "\\d{s}";
        break;
      case 0x022e:
        os << "\\.{O}";
        break;
      case 0x022f:
        os << "\\.{o}";
        break;
      case 0x3008:
        os << "$\\langle$";
        break;
      case 0x3009:
        os << "$\\rangle$";
        break;
      case 0x2022:
        os << "\\textbullet{}";
        break;

      case 0x2013:
        os << "--";
        break;
      case 0x2014:
        os << "---";
        break;
      case 0x201c:
        os << "\\ldqo{}";
        break;
      case 0x201d:
        os << "\\rdqo{}";
        break;
      case 0x2018:
        os << "\\lsqo{}";
        break;
      case 0x2019:
        os << "\\rsqo{}";
        break;
      case 0x2026:
        os << "\\hellip{}";
        break;
      case 0x202f:
        os << "\\thsp{}";
        break;

      case 0x2190:
        os << "$\\leftarrow{}$";
        break;
      case 0x2192:
        os << "$\\rightarrow{}$";
        break;
      case 0x21d2:
        os << "$\\Rightarrow{}$";
        break;
      case 0x21a6:
        os << "$\\mapsto{}$";
        break;
      case 0x2261:
        os << "$\\equiv{}$";
        break;
      case 0x22a3:
        os << "$\\dashv{}$";
        break;
      case 0x1ebd:
        os << "\\~e";
        break;
      case 0x27e8:
        os << "$\\langle{}$";
        break;
      case 0x27e9:
        os << "$\\rangle{}$";
        break;
        // case 0x2772:
        //   break;
        // case 0x2773:
        //   break;

      case 0x0127:
        os << "\\textipa{\\textcrh}";
        po->_need_tipa = true;
        break;
      case 0x014b:
        os << "\\textipa{N}";
        po->_need_tipa = true;
        break;
      case 0x0188:
        os << "\\textipa{\\texthtc}";
        po->_need_tipa = true;
        break;
      case 0x0199:
        os << "\\textipa{\\texthtk}";
        po->_need_tipa = true;
        break;
      case 0x01a5:
        os << "\\textipa{\\texthtp}";
        po->_need_tipa = true;
        break;
      case 0x01ad:
        os << "\\textipa{\\texthtt}";
        po->_need_tipa = true;
        break;
      case 0x0250:
        os << "\\textipa{5}";
        po->_need_tipa = true;
        break;
      case 0x0251:
        os << "\\textipa{A}";
        po->_need_tipa = true;
        break;
      case 0x0252:
        os << "\\textipa{6}";
        po->_need_tipa = true;
        break;
      case 0x0253:
        os << "\\textipa{\\!b}";
        po->_need_tipa = true;
        break;
      case 0x0254:
        os << "\\textipa{O}";
        po->_need_tipa = true;
        break;
      case 0x0255:
        os << "\\textipa{C}";
        po->_need_tipa = true;
        break;
      case 0x0256:
        os << "\\textipa{\\:d}";
        po->_need_tipa = true;
        break;
      case 0x0257:
        os << "\\textipa{\\!d}";
        po->_need_tipa = true;
        break;
      case 0x0259:
        os << "\\textipa{@}";
        po->_need_tipa = true;
        break;
      case 0x025b:
        os << "\\textipa{E}";
        po->_need_tipa = true;
        break;
      case 0x025c:
        os << "\\textipa{3}";
        po->_need_tipa = true;
        break;
      case 0x025e:
        os << "\\textipa{\\textcloserevepsilon}";
        po->_need_tipa = true;
        break;
      case 0x025f:
        os << "\\textipa{\\textbardotlessj}";
        po->_need_tipa = true;
        break;
      case 0x0260:
        os << "\\textipa{\\!g}";
        po->_need_tipa = true;
        break;
      case 0x0261:
        os << "\\textipa{g}";
        po->_need_tipa = true;
        break;
      case 0x0262:
        os << "\\textipa{\\;G}";
        po->_need_tipa = true;
        break;
      case 0x0263:
        os << "\\textipa{G}";
        po->_need_tipa = true;
        break;
      case 0x0264:
        os << "\\textipa{7}";
        po->_need_tipa = true;
        break;
      case 0x0265:
        os << "\\textipa{\\textlhtlongy}";
        po->_need_tipa = true;
        break;
      case 0x0266:
        os << "\\textipa{H}";
        po->_need_tipa = true;
        break;
      case 0x0267:
        os << "\\textipa{\\texththeng}";
        po->_need_tipa = true;
        break;
      case 0x0268:
        os << "\\textipa{1}";
        po->_need_tipa = true;
        break;
      case 0x026a:
        os << "\\textipa{I}";
        po->_need_tipa = true;
        break;
      case 0x026b:
        os << "\\textipa{\\|~l}";
        po->_need_tipa = true;
        break;
      case 0x026c:
        os << "\\textipa{\\textbeltl}";
        po->_need_tipa = true;
        break;
      case 0x026d:
        os << "\\textipa{\\:l}";
        po->_need_tipa = true;
        break;
      case 0x026e:
        os << "\\textipa{\\textlyoghlig}";
        po->_need_tipa = true;
        break;
      case 0x026f:
        os << "\\textipa{W}";
        po->_need_tipa = true;
        break;
      case 0x0270:
        os << "\\textipa{\\textturnmrleg}";
        po->_need_tipa = true;
        break;
      case 0x0271:
        os << "\\textipa{M}";
        po->_need_tipa = true;
        break;
      case 0x0272:
        os << "\\textipa{\\textltailn}";
        po->_need_tipa = true;
        break;
      case 0x0273:
        os << "\\textipa{\\:n}";
        po->_need_tipa = true;
        break;
      case 0x0274:
        os << "\\textipa{\\;N}";
        po->_need_tipa = true;
        break;
      case 0x0275:
        os << "\\textipa{8}";
        po->_need_tipa = true;
        break;
      case 0x0277:
        os << "\\textipa{\\textcloseomega}";
        po->_need_tipa = true;
        break;
      case 0x0279:
        os << "\\textipa{\\*r}";
        po->_need_tipa = true;
        break;
      case 0x027a:
        os << "\\textipa{\\textturnlonglegr}";
        po->_need_tipa = true;
        break;
      case 0x027b:
        os << "\\textipa{\\:R}";
        po->_need_tipa = true;
        break;
      case 0x027d:
        os << "\\textipa{\\:r}";
        po->_need_tipa = true;
        break;
      case 0x027e:
        os << "\\textipa{R}";
        po->_need_tipa = true;
        break;
      case 0x0280:
        os << "\\textipa{\\;R}";
        po->_need_tipa = true;
        break;
      case 0x0281:
        os << "\\textipa{K}";
        po->_need_tipa = true;
        break;
      case 0x0282:
        os << "\\textipa{\\:s}";
        po->_need_tipa = true;
        break;
      case 0x0283:
        os << "\\textipa{S}";
        po->_need_tipa = true;
        break;
      case 0x0284:
        os << "\\textipa{\\texthtbardotlessjvar}";
        po->_need_tipa = true;
        break;
      case 0x0288:
        os << "\\textipa{\\:t}";
        po->_need_tipa = true;
        break;
      case 0x0289:
        os << "\\textipa{\\textbaru}";
        po->_need_tipa = true;
        break;
      case 0x028a:
        os << "\\textipa{U}";
        po->_need_tipa = true;
        break;
      case 0x028b:
        os << "\\textipa{V}";
        po->_need_tipa = true;
        break;
      case 0x028c:
        os << "\\textipa{2}";
        po->_need_tipa = true;
        break;
      case 0x028d:
        os << "\\textipa{\\*w}";
        po->_need_tipa = true;
        break;
      case 0x028e:
        os << "\\textipa{L}";
        po->_need_tipa = true;
        break;
      case 0x028f:
        os << "\\textipa{Y}";
        po->_need_tipa = true;
        break;
      case 0x0290:
        os << "\\textipa{\\:z}";
        po->_need_tipa = true;
        break;
      case 0x0291:
        os << "\\textipa{\\textctz}";
        po->_need_tipa = true;
        break;
      case 0x0292:
        os << "\\textipa{Z}";
        po->_need_tipa = true;
        break;
      case 0x0294:
        os << "\\textipa{P}";
        po->_need_tipa = true;
        break;
      case 0x0295:
        os << "\\textipa{Q}";
        po->_need_tipa = true;
        break;
      case 0x0299:
        os << "\\textipa{\\;B}";
        po->_need_tipa = true;
        break;
      case 0x029C:
        os << "\\textipa{\\;H}";
        po->_need_tipa = true;
        break;
      case 0x029a:
        os << "\\textipa{\\textcloserevepsilon}";
        po->_need_tipa = true;
        break;
      case 0x029b:
        os << "\\textipa{\\!G}";
        po->_need_tipa = true;
        break;
      case 0x029d:
        os << "\\textipa{J}";
        po->_need_tipa = true;
        break;
      case 0x029f:
        os << "\\textipa{\\;L}";
        po->_need_tipa = true;
        break;
      case 0x02a0:
        os << "\\textipa{\\texthtq}";
        po->_need_tipa = true;
        break;
      case 0x02a1:
        os << "\\textipa{\\textbarglotstop}";
        po->_need_tipa = true;
        break;
      case 0x02a2:
        os << "\\textipa{\\textbarrevglotstop}";
        po->_need_tipa = true;
        break;
      case 0x02d0:
        os << "\\textipa{:}";
        po->_need_tipa = true;
      case 0x02e0:
        os << "\\textipa{\\textbabygamma}";
        po->_need_tipa = true;
        break;
      case 0x03b2:
        os << "\\textipa{B}";
        po->_need_tipa = true;
        break;
      case 0x03b8:
        os << "\\textipa{T}";
        po->_need_tipa = true;
        break;

      case '\n':
        switch (ctx._wrapstyle) {
        case tex_detail::k_asis_wrap:
          os << "\\\\\\relax" << std::endl;
          break;
        case tex_detail::k_normal_wrap:
          os << " ";
          break;
        case tex_detail::k_no_wrap:
          os << " ";
          break;
        }
        break;

      case ' ':
      case '\t':
        switch (ctx._wstreatment) {
        case tex_detail::k_preserve_ws:
          os << "\\ws{}";
          break;
        case tex_detail::k_collapse_ws:
          os << ' ';
          break;
        case tex_detail::k_ignore_ws:
          break;
        }
        break;

      case '\r':
        break;

      default:
        os << utils::u32string_to_utf8(std::u32string() + c);
      }

      os << post_op.str();

      cidx = next_idx;
    }
  }


  void vspace(TexProcessor* po, const IFormattingObject* fo, const char* property_name,
              const char* keep_prop_name) {
    auto nobreak = po->property_or_none<bool>(fo, keep_prop_name);
    auto val = po->property_or_none<fo::LengthSpec>(fo, property_name);

    if (val && val->_value != 0) {
      if (nobreak && *nobreak)
        po->stream() << "\\nobreak" << std::endl;
      if (val->_conditionalp)
        po->stream() << "\\vsc{" << dimen2str(*val) << "}";
      else
        po->stream() << "\\vs{" << dimen2str(*val) << "}";
      if (nobreak && *nobreak)
        po->stream() << "\\nobreak" << std::endl;
    }
    else if (nobreak && *nobreak)
      po->stream() << "\\nobreak" << std::endl;
  }


  void enc_fontsize(TexProcessor* po, const IFormattingObject* fo) {
    auto fs = po->property(fo, "font-size", fo::LengthSpec(fo::kDimen, 10.0, fo::k_pt));
    auto ls = po->property(fo, "line-spacing",
                           fo::LengthSpec(fo::kDimen, fs._value * 1.2, fo::k_pt));
    // auto posptshift = po->property(fo, "position-point-shift",
    //                                fo::LengthSpec(fo::kDimen, 0.0, fo::k_pt));
    auto fontcaps = po->property(fo, "font-caps", std::string("normal"));

    if (fontcaps == "normal")
      po->style_ctx()._capsstyle = tex_detail::k_normal_caps;
    else if (fontcaps == "lower")
      po->style_ctx()._capsstyle = tex_detail::k_lower_caps;
    else if (fontcaps == "caps")
      po->style_ctx()._capsstyle = tex_detail::k_upper_caps;
    else if (fontcaps == "small-caps")
      po->style_ctx()._capsstyle = tex_detail::k_small_caps;

    po->stream() << "\\fns{" << dimen2str(fs) << "}{" << dimen2str(ls) << "}";

    if (auto fw = po->property_or_none<std::string>(fo, "font-weight")) {
      if (*fw == "bold")
        po->stream() << "\\bfseries{}";
      else if (*fw == "medium")
        po->stream() << "\\mdseries{}";
    }

    if (auto fp = po->property_or_none<std::string>(fo, "font-posture")) {
      if (*fp == "italic")
        po->stream() << "\\itshape{}";
      else if (*fp == "oblique")
        po->stream() << "\\slshape{}";
      else if (*fp == "upright")
        po->stream() << "\\upshape{}";
    }

    if (auto fn = po->property_or_none<std::string>(fo, "font-name")) {
      if (*fn == "monospace")
        po->stream() << "\\ttfamily{}";
      else if (*fn == "sans-serif")
        po->stream() << "\\sffamily{}";
      else if (*fn == "roman")
        po->stream() << "\\rmfamily{}";
      else if (*fn == "ipa") {
        po->stream() << "\\tipaencoding{}";
        po->_need_tipa = true;
      }
    }
  }


  void enc_paraprops(TexProcessor* po, const IFormattingObject* fo) {
    auto max_inf = std::numeric_limits<double>::infinity();

    auto ind = dimen2str(po->property(fo, "first-line-start-indent",
                                      fo::LengthSpec(fo::kInline, 0.0, fo::k_pt)));
    auto ext =
      dimen2str(po->property(fo, "last-line-end-indent",
                             fo::LengthSpec(fo::kInline, 1.0, fo::k_em, 1.0, max_inf)));
    auto sind = dimen2str(
      po->property(fo, "start-indent", fo::LengthSpec(fo::kInline, 0.0, fo::k_pt)));
    auto eind = dimen2str(
      po->property(fo, "end-indent", fo::LengthSpec(fo::kInline, 0.0, fo::k_pt)));

    po->stream() << "\\ps{" << ind << "}{" << ext << "}{" << sind << "}{" << eind << "}";
  }


  void enc_color(TexProcessor* po, const fo::Color co) {
    switch (co._space) {
    case fo::kRGB:
      po->stream() << "\\color[rgb]{" << co._rgb._red << "," << co._rgb._green << ","
                   << co._rgb._blue << "}";
      break;
    case fo::kCMYK:
      po->stream() << "\\color[cmyk]{" << co._cmyk._cyan << "," << co._cmyk._magenta
                   << "," << co._cmyk._yellow << "," << co._cmyk._black << "}";
      break;
    case fo::kGray:
      po->stream() << "\\color[gray]{" << co._gray << "}";
      break;
    }
  }


  class TexLiteralFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      po->finalize_breaks();
      auto str = static_cast<const fo::Literal*>(fo)->text();

      switch (po->style_ctx()._capsstyle) {
      case tex_detail::k_normal_caps:
        escape_str_to_stream(po, po->stream(), str, po->style_ctx());
        break;
      case tex_detail::k_lower_caps:
        escape_str_to_stream(po, po->stream(), utils::to_lower(str), po->style_ctx());
        break;
      case tex_detail::k_upper_caps:
        escape_str_to_stream(po, po->stream(), utils::to_upper(str), po->style_ctx());
        break;
      case tex_detail::k_small_caps:
        po->stream() << "{\\sc ";
        escape_str_to_stream(po, po->stream(), str, po->style_ctx());
        po->stream() << "}";
        break;
      default:
        assert(false);
      }
    }
  };


  class TexParagraphFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      po->finalize_breaks();

      auto lastctx = po->style_ctx();

      vspace(po, fo, "space-before", "keep-with-previous?");

      auto debug_info = po->property_or_none<std::string>(fo, "metadata.debug-info");
      if (debug_info)
        po->stream() << "%%debug: " << *debug_info << std::endl;

      po->stream() << "\\para{";
      enc_paraprops(po, fo);

      auto linesprops = po->property(fo, "lines", std::string("wrap"));
      if (linesprops == "asis")
        po->style_ctx()._wrapstyle = tex_detail::k_asis_wrap;

      auto wstreatment =
        po->property(fo, "whitespace-treatment", std::string("collapse"));
      if (wstreatment == "preserve")
        po->style_ctx()._wstreatment = tex_detail::k_preserve_ws;
      else if (wstreatment == "collapse")
        po->style_ctx()._wstreatment = tex_detail::k_collapse_ws;
      else if (wstreatment == "ignore")
        po->style_ctx()._wstreatment = tex_detail::k_ignore_ws;

      po->stream() << "}{";
      enc_fontsize(po, fo);

      auto quadding = po->property(fo, "quadding", std::string("left"));
      if (quadding == "left")
        po->stream() << "\\quadleft{}";
      else if (quadding == "right")
        po->stream() << "\\quadright{}";
      else if (quadding == "center")
        po->stream() << "\\quadcenter{}";

      po->push_delayed_anchors();
      po->render_sosofo(&fo->port("text"));

      if (wstreatment == "preserve") {
        po->style_ctx()._wstreatment = tex_detail::k_preserve_ws;
        po->stream() << "\\vspace{-1\\baselineskip}";
      }
      else if (wstreatment == "collapse")
        po->style_ctx()._wstreatment = tex_detail::k_collapse_ws;
      else if (wstreatment == "ignore")
        po->style_ctx()._wstreatment = tex_detail::k_ignore_ws;

      po->stream() << "}";
      if (debug_info)
        po->stream() << "%%debug: " << *debug_info << std::endl;

      vspace(po, fo, "space-after", "keep-with-next?");

      po->style_ctx() = lastctx;

      po->stream() << std::endl;
    }
  };


  class TexParagraphBreakFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      po->finalize_breaks();
      po->stream() << std::endl << "\\newline{}" << std::endl;
    }
  };


  class TexDisplayGroupFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      if (auto break_before = po->property_or_none<std::string>(fo, "break-before?")) {
        if (*break_before == "page")
          po->request_page_break(tex_detail::kBreakPageBefore);
      }

      vspace(po, fo, "space-before", "keep-with-previous?");

      auto& text_port = fo->port("text");
      if (text_port.length() > 0) {
        po->render_sosofo(&text_port);
      }
      else {
        po->push_delayed_anchors();
        po->finalize_breaks();
      }

      vspace(po, fo, "space-after", "keep-with-next?");

      if (auto break_after = po->property_or_none<std::string>(fo, "break-after?")) {
        if (*break_after == "page")
          po->request_page_break(tex_detail::kBreakPageAfter);
      }
    }
  };


  class TexBoxFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      // Simple passthru for now
      auto& text_port = fo->port("text");
      if (text_port.length() > 0) {
        po->render_sosofo(&text_port);
      }
    }
  };


  class TexSimplePageSequenceFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      if (!po->_first_page) {
        po->stream() << std::endl << "\\newpage{}" << std::endl << std::endl;
        po->request_page_break(tex_detail::kBreakPageBefore);
      }
      else {
        po->_first_page = false;
      }
      po->request_page_break(tex_detail::kNoBreak);

      auto title = po->property(fo, "metadata.title", std::string());
      auto author = po->property(fo, "metadata.author", std::string());
      auto desc = po->property(fo, "metadata.desc", std::string());
      auto cover = po->property(fo, "metadata.cover", std::string());

      auto pagewidth = po->property(fo, "page-width", po->paper_width());
      auto pageheight = po->property(fo, "page-height", po->paper_height());
      auto leftmargin =
        po->property(fo, "left-margin", fo::LengthSpec(fo::kDimen, 20, fo::k_mm));
      auto rightmargin =
        po->property(fo, "right-margin", fo::LengthSpec(fo::kDimen, 20, fo::k_mm));
      auto topmargin =
        po->property(fo, "top-margin", fo::LengthSpec(fo::kDimen, 20, fo::k_mm));
      auto bottommargin =
        po->property(fo, "bottom-margin", fo::LengthSpec(fo::kDimen, 30, fo::k_mm));
      auto headermargin =
        po->property(fo, "header-margin", fo::LengthSpec(fo::kDimen, 10, fo::k_mm));
      // auto footermargin = po->property(fo, "footer-margin",
      //                                  fo::LengthSpec(fo::kDimen, 20, fo::k_mm));
      auto startmargin =
        po->property(fo, "start-margin", fo::LengthSpec(fo::kDimen, 0, fo::k_pt));
      po->_current_start_margin = startmargin;
      auto endmargin =
        po->property(fo, "end-margin", fo::LengthSpec(fo::kDimen, 0, fo::k_pt));

      po->stream() << "\\setlength\\paperwidth{" << dimen2str(pagewidth) << "}"
                   << std::endl;
      po->stream() << "\\setlength\\paperheight{" << dimen2str(pageheight) << "}"
                   << std::endl;

      po->stream() << "\\setlength\\textwidth{\\paperwidth}" << std::endl;
      po->stream() << "\\setlength\\oddsidemargin{" << dimen2str(leftmargin) << "}"
                   << std::endl
                   << "\\setlength\\evensidemargin{" << dimen2str(leftmargin) << "}"
                   << std::endl
                   << "\\addtolength{\\textwidth}{-" << dimen2str(leftmargin) << "}"
                   << std::endl;

      po->stream() << "\\addtolength{\\textwidth}{-" << dimen2str(rightmargin) << "}"
                   << std::endl;

      po->stream() << "\\setlength{\\hoffset}{" << dimen2str(po->paper_width()) << "}"
                   << std::endl
                   << "\\addtolength{\\hoffset}{-" << dimen2str(pagewidth) << "}"
                   << std::endl
                   << "\\divide\\hoffset by 2"
                   << std::endl
                   // to correct tex's auto hoffset of 1in
                   << "\\addtolength{\\hoffset}{-1in}" << std::endl;

      po->stream() << "\\setlength\\textheight{\\paperheight}" << std::endl
                   << "\\addtolength{\\textheight}{-" << dimen2str(topmargin) << "}"
                   << std::endl
                   << "\\addtolength{\\textheight}{-" << dimen2str(bottommargin) << "}"
                   << std::endl;

      // dsssl:topmargin = \voffset + \topmargin + \headheight + \headersep
      // dsssl:headermargin = \voffset + \topmargin + \headheight
      // -> dsssl:topmargin = dsssl:headermargin + \headersep
      // -> \headersep = dsssl:topmargin - dsssl:headermargin
      //
      // \topmargin = dsssl:topmargin - \headheigth - \headersep
      // \headheight => ? ;; can't be computed from the dsssl
      // characteristics.  Let's assume 12pt (=1\baselineskip)
      po->stream() << "\\setlength\\headsep{" << dimen2str(topmargin) << "}" << std::endl
                   << "\\addtolength\\headsep{-" << dimen2str(headermargin) << "}"
                   << std::endl
                   << "\\setlength\\headheight{12pt}" << std::endl
                   << "\\setlength\\topmargin{" << dimen2str(topmargin) << "}"
                   << std::endl
                   << "\\addtolength\\topmargin{-\\headsep}" << std::endl
                   << "\\addtolength\\topmargin{-\\headheight}" << std::endl;

      // let's center the page on the paper
      // \voffset = dsssl:pageheight / 2
      po->stream() << "\\setlength\\voffset{" << dimen2str(po->paper_height()) << "}"
                   << std::endl
                   << "\\addtolength{\\voffset}{-" << dimen2str(pageheight) << "}"
                   << std::endl
                   << "\\divide\\voffset by 2"
                   << std::endl
                   // to correct tex's auto voffset of 1in
                   << "\\addtolength{\\voffset}{-1in}" << std::endl;

      po->stream() << "\\setlength{\\leftskip}{" << dimen2str(startmargin) << "}"
                   << std::endl
                   << "\\setlength{\\rightskip}{" << dimen2str(endmargin) << "}"
                   << std::endl;

      po->stream() << "\\changelayout%%" << std::endl;

      //--------
      auto leftheader = po->property_or_none<std::shared_ptr<Sosofo>>(fo, "left-header");
      auto centerheader =
        po->property_or_none<std::shared_ptr<Sosofo>>(fo, "center-header");
      auto rightheader =
        po->property_or_none<std::shared_ptr<Sosofo>>(fo, "right-header");

      if (leftheader || rightheader || centerheader) {
        po->stream() << "\\makeheadline{";
        if (leftheader)
          po->render_sosofo(leftheader->get());
        po->stream() << "}{";
        if (centerheader)
          po->render_sosofo(centerheader->get());
        po->stream() << "}{";
        if (rightheader)
          po->render_sosofo(rightheader->get());
        po->stream() << "}";
      }

      auto leftfooter = po->property_or_none<std::shared_ptr<Sosofo>>(fo, "left-footer");
      auto centerfooter =
        po->property_or_none<std::shared_ptr<Sosofo>>(fo, "center-footer");
      auto rightfooter =
        po->property_or_none<std::shared_ptr<Sosofo>>(fo, "right-footer");

      if (leftfooter || rightfooter || centerfooter) {
        po->stream() << "\\makefootline{";
        if (leftfooter)
          po->render_sosofo(leftfooter->get());
        po->stream() << "}{";
        if (centerfooter)
          po->render_sosofo(centerfooter->get());
        po->stream() << "}{";
        if (rightfooter)
          po->render_sosofo(rightfooter->get());
        po->stream() << "}";
      }

      po->stream() << "\\hsize=\\textwidth" << std::endl;

      po->stream() << "\\def\\fnlayout{%%" << std::endl;
      po->stream() << "\\setlength{\\leftskip}{" << dimen2str(startmargin) << "}%%"
                   << std::endl;
      po->stream() << "\\setlength{\\rightskip}{" << dimen2str(endmargin) << "}}%%"
                   << std::endl;
      // an anchor to make vspaces on start pages possible
      po->stream() << "\\hbox{}\\par\\vs{-2\\baselineskip}%%" << std::endl;

      po->request_page_break(tex_detail::kSurpressNextPageBreak);
      po->render_sosofo(&fo->port("text"));
    }
  };


  class TexSequenceFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      po->finalize_breaks();

      auto lastctx = po->style_ctx();

      po->stream() << "{";

      auto pps = po->property_or_none<fo::LengthSpec>(fo, "position-point-shift");

      if (pps) {
        if (pps->_value < 0)
          po->stream() << "\\h{";
        else if (pps->_value > 0)
          po->stream() << "\\t{";
      }

      enc_fontsize(po, fo);
      if (auto co = po->property_or_none<fo::Color>(fo, "color"))
        enc_color(po, *co);
      po->render_sosofo(&fo->port("text"));

      if (pps) {
        if (pps->_value != 0)
          po->stream() << "}";
      }

      po->stream() << "}";

      po->style_ctx() = lastctx;
    }
  };


  const auto k_left = std::string("left");
  const auto k_center = std::string("center");
  const auto k_right = std::string("right");

  class TexLineFieldFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      po->finalize_breaks();

      auto field_width = po->property_or_none<fo::LengthSpec>(fo, "field-width");
      auto field_align = po->property_or_none<std::string>(fo, "field-align");

      auto lastctx = po->style_ctx();

      if (field_width && field_width->_value > 0) {
        if (field_width->_max != field_width->_value) {
          po->stream() << "{";
          enc_fontsize(po, fo);

          auto left_hss =
            field_align && (*field_align == "right" || *field_align == "center")
              ? "\\hfill"
              : "";
          auto right_hss =
            field_align && (*field_align == "left" || *field_align == "center")
              ? "\\hfill"
              : "";
          po->stream() << "\\lf{" << exact_dimen2str(*field_width) << "}{"
                       << max_dimen2str(*field_width) << "}{" << left_hss << "}{"
                       << right_hss << "}{";
          po->render_sosofo(&fo->port("text"));

          po->stream() << "}}";
          return;
        }
        else
          po->stream() << "\\hbox to " << exact_dimen2str(*field_width) << "{";
      }
      else
        po->stream() << "{";

      if (field_align && (*field_align == "right" || *field_align == "center"))
        po->stream() << "\\hfill{}";

      enc_fontsize(po, fo);
      po->render_sosofo(&fo->port("text"));

      if (field_align && (*field_align == "left" || *field_align == "center"))
        po->stream() << "\\hfill{}";
      po->stream() << "}";

      po->style_ctx() = lastctx;
    }
  };


  class TexPageNumberFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      po->finalize_breaks();

      auto refid = po->property(fo, "refid", std::string("#current"));
      if (refid == "#current")
        po->stream() << "\\pageno{}";
      else if (!refid.empty())
        po->stream() << "\\pageref{" << refid << "}";
    }
  };


  class TexScoreFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      po->finalize_breaks();

      // const auto color = po->property_or_none<fo::Color>(fo, "color");
      const auto thickness = po->property_or_none<fo::LengthSpec>(fo, "line-thickness");
      const auto type = po->property(fo, "score-type", std::string("none"));

      if (type == "above") {
        // no supported
      }
      else if (type == "below") {
        po->stream() << "\\noindent\\hrulefill ";
      }
      else if (type == "through") {
        // no supported
      }
    }
  };


  class TexLinkFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      // Simple passthru for now
      auto& text_port = fo->port("text");
      if (text_port.length() > 0) {
        po->render_sosofo(&text_port);
      }
    }
  };


  class TexAnchorFoProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      po->finalize_breaks();

      if (auto id = po->property_or_none<std::string>(fo, "id")) {
        if (!id->empty()) {
          po->_delayed_anchors.emplace_back(std::string("\\label{") + *id + "}");
        }
      }
    }
  };


  class TexSimpleColumnSetSequenceProcessor : public IFoProcessor<TexProcessor>
  {
  public:
    void render(TexProcessor* po, const IFormattingObject* fo) const override {
      po->finalize_breaks();

      auto col_num = po->property(fo, "column-number", 1);

      auto gutterwidth =
        po->property(fo, "gutter-width", fo::LengthSpec(fo::kDimen, 0, fo::k_pt));

      po->stream() << "{\\leftskip0pt\\relax%" << std::endl
                   << " \\setlength{\\columnsep}{" << dimen2str(gutterwidth) << "}%"
                   << std::endl;
      po->stream() << "\\begin{tbmulticols}{" << col_num << "}"
                   << "{" << dimen2str(po->_current_start_margin) << "}"
                   << "{-" << dimen2str(po->_current_start_margin) << "}" << std::endl;
      po->render_sosofo(&fo->port("text"));
      po->stream() << "\\end{tbmulticols}}" << std::endl;
    }
  };
} // namespace


TexProcessor::TexProcessor()
  : _verbose(false)
  , _style_ctx{tex_detail::k_normal_caps, tex_detail::k_normal_wrap,
               tex_detail::k_collapse_ws}
  , _cropmarks(tex_detail::kOff)
  , _paper_dimen(std::make_tuple(fo::LengthSpec(fo::kDimen, 210, fo::k_mm),
                                 fo::LengthSpec(fo::kDimen, 297, fo::k_mm),
                                 std::string("a4"))) {}


TexProcessor::TexProcessor(const cxxopts::ParseResult& args)
  : TexProcessor() {
  _verbose = args.count("verbose") != 0;

  auto cropmarks =
    args.count("cropmarks") ? args["cropmarks"].as<std::string>() : std::string("off");
  if (cropmarks == "cam")
    _cropmarks = tex_detail::kCamera;
  else if (cropmarks == "frame")
    _cropmarks = tex_detail::kFrame;
  else
    _cropmarks = tex_detail::kOff;

  auto ps =
    args.count("paper-size") ? args["paper-size"].as<std::string>() : std::string("a4");

  if (ps == "a3")
    _paper_dimen = std::make_tuple(fo::LengthSpec(fo::kDimen, 297, fo::k_mm),
                                   fo::LengthSpec(fo::kDimen, 420, fo::k_mm), ps);
  else if (ps == "a4")
    _paper_dimen = std::make_tuple(fo::LengthSpec(fo::kDimen, 210, fo::k_mm),
                                   fo::LengthSpec(fo::kDimen, 297, fo::k_mm), ps);
  else if (ps == "a5")
    _paper_dimen = std::make_tuple(fo::LengthSpec(fo::kDimen, 148, fo::k_mm),
                                   fo::LengthSpec(fo::kDimen, 210, fo::k_mm), ps);
  else if (ps == "letter")
    _paper_dimen = std::make_tuple(fo::LengthSpec(fo::kDimen, 8.5, fo::k_in),
                                   fo::LengthSpec(fo::kDimen, 11, fo::k_in), ps);
  else if (ps == "legal")
    _paper_dimen = std::make_tuple(fo::LengthSpec(fo::kDimen, 8.5, fo::k_in),
                                   fo::LengthSpec(fo::kDimen, 14, fo::k_in), ps);
  else {
    std::smatch user_dimen;
    std::regex_search(ps, user_dimen,
                      std::regex(std::regex("([[:digit:]]+)([[:alpha:]]+)x"
                                            "([[:digit:]]+)([[:alpha:]]+)")));
    if (!user_dimen.empty()) {
      _paper_dimen =
        std::make_tuple(fo::LengthSpec(fo::kDimen, std::stof(user_dimen[1].str()),
                                       unit_str2unit(user_dimen[2].str())),
                        fo::LengthSpec(fo::kDimen, std::stof(user_dimen[3].str()),
                                       unit_str2unit(user_dimen[4].str())),
                        std::string("user"));
    }
  }
}


void TexProcessor::add_program_options(cxxopts::Options& options) const {
  options.add_options("Tex renderer")
    // clang-format off
    ("cropmarks", "enable crop marks style: cam, frame, off.  Default is off",
                  cxxopts::value<std::string>())
    ("paper-size", "give size of paper use.  Default is 'a4'",
                  cxxopts::value<std::string>()->default_value("a4"))
    ;
  // clang-format on
}


const IFoProcessor<TexProcessor>*
TexProcessor::lookup_fo_processor(const std::string& fo_classname) const {
  static auto procs = std::map<std::string, std::shared_ptr<IFoProcessor<TexProcessor>>>{
    {"#literal", std::make_shared<TexLiteralFoProcessor>()},
    {"#paragraph", std::make_shared<TexParagraphFoProcessor>()},
    {"#paragraph-break", std::make_shared<TexParagraphBreakFoProcessor>()},
    {"#display-group", std::make_shared<TexDisplayGroupFoProcessor>()},
    {"#box", std::make_shared<TexBoxFoProcessor>()},
    {"#simple-page-sequence", std::make_shared<TexSimplePageSequenceFoProcessor>()},
    {"#sequence", std::make_shared<TexSequenceFoProcessor>()},
    {"#line-field", std::make_shared<TexLineFieldFoProcessor>()},
    {"#page-number", std::make_shared<TexPageNumberFoProcessor>()},
    {"#anchor", std::make_shared<TexAnchorFoProcessor>()},
    {"#link", std::make_shared<TexLinkFoProcessor>()},
    {"#score", std::make_shared<TexScoreFoProcessor>()},
    {"#simple-column-set-sequence",
     std::make_shared<TexSimpleColumnSetSequenceProcessor>()},
  };

  auto i_find = procs.find(fo_classname);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void TexProcessor::before_rendering() {
  _output_file_tmp = _output_file;
  _output_file_tmp += ".tmp";
  _file = fs::File(_output_file_tmp.string());

  std::error_code ec;
  _file.open(std::ios_base::in | std::ios_base::out | std::ios_base::binary |
               std::ios_base::trunc,
             ec);
  if (ec) {
    std::cerr << "Opening file '" << _output_file_tmp << "' failed" << std::endl;
  }

  _file.stream() << "\\begin{document}" << std::endl;
}


void TexProcessor::after_rendering() {
  stream() << "\\end{document}" << std::endl;
  stream().seekg(0);

  // now write the final file; we have now all info which features we've to use.
  std::error_code ec;
  filesystem::with_stream_for_writing(
    _output_file, ec,
    [&](std::ostream& os) {
      //_stream << "\\usepackage{listings}" << std::endl
      os << "\\documentclass{textbook}" << std::endl
         << "\\usepackage[utf8]{inputenc}" << std::endl;
      switch (_cropmarks) {
      case tex_detail::kCamera:
        os << "\\newcommand*\\infofont[1]{{\\textsf{\\fontsize{7}{8.5}"
              "\\selectfont#1}}}"
           << std::endl
           << "\\usepackage[cam," << crop_paper_size(_paper_dimen)
           << ",horigin=0in,vorigin=0in,font=infofont]{crop}" << std::endl;
        break;
      case tex_detail::kFrame:
        os << "\\usepackage[frame," << crop_paper_size(_paper_dimen)
           << ",horigin=0in,vorigin=0in,noinfo]{crop}" << std::endl;
        break;
      case tex_detail::kOff:
        os << "\\usepackage[off," << crop_paper_size(_paper_dimen)
           << ",horigin=0in,vorigin=0in,noinfo]{crop}" << std::endl;
        break;
      }

      if (_need_tipa) {
        os << "\\usepackage{tipa}" << std::endl;
      }

      os << stream().rdbuf();
    },
    std::ios_base::out | std::ios_base::binary | std::ios_base::trunc);
  if (ec) {
    std::cerr << "Opening file '" << _output_file << "' failed" << std::endl;
  }

  _file.close(ec);
  if (ec) {
    std::cerr << "Closing file '" << _output_file << "' failed" << std::endl;
  }
}


void TexProcessor::finalize_breaks() {
  switch (_break_pending) {
  case tex_detail::kSurpressNextPageBreak:
  case tex_detail::kNoBreak:
    _break_pending = tex_detail::kNoBreak;
    break;
  case tex_detail::kBreakPageBefore:
  case tex_detail::kBreakPageAfter:
    stream() << std::endl
             << "\\newpage{}" << std::endl
             << "\\hbox{}\\par\\vs{-2\\baselineskip}%%" << std::endl
             << std::endl;
    _break_pending = tex_detail::kNoBreak;
    break;
  }
}


void TexProcessor::request_page_break(tex_detail::BreakKind breakKind) {
  if (breakKind != _break_pending &&
      _break_pending != tex_detail::kSurpressNextPageBreak) {
    _break_pending = breakKind;
  }
}


fo::LengthSpec TexProcessor::paper_width() const {
  return std::get<0>(_paper_dimen);
}


fo::LengthSpec TexProcessor::paper_height() const {
  return std::get<1>(_paper_dimen);
}


void TexProcessor::push_delayed_anchors() {
  for (const auto& s : _delayed_anchors) {
    stream() << s;
  }
  _delayed_anchors.clear();
}

} // namespace eyestep
