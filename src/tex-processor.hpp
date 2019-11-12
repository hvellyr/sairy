// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "abstract-processor.hpp"
#include "fo.hpp"

#include "program_options/program_options.hpp"

#include "fspp/filesystem.hpp"
#include "fspp/utils.hpp"

#include <istream>
#include <list>
#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <tuple>
#include <unordered_map>


namespace eyestep {

class Sosofo;
class IFormattingObject;
template <typename T>
class IFoProcessor;

namespace tex_detail {
  enum CapsStyle
  {
    k_normal_caps,
    k_lower_caps,
    k_upper_caps,
    k_small_caps,
  };

  enum WrapStyle
  {
    k_normal_wrap,
    k_asis_wrap,
    k_no_wrap,
  };

  enum WsTreatment
  {
    k_preserve_ws,
    k_collapse_ws,
    k_ignore_ws,
  };

  struct TexStyleContext
  {
    CapsStyle _capsstyle;
    WrapStyle _wrapstyle;
    WsTreatment _wstreatment;
  };

  enum BreakKind
  {
    kNoBreak,
    kBreakPageBefore,
    kBreakPageAfter,
    kSurpressNextPageBreak,
  };

  enum CropMarksKind
  {
    kOff,
    kCamera,
    kFrame,
  };
} // namespace tex_detail


class TexProcessor : public AbstractProcessor<TexProcessor>
{
  bool _verbose;
  filesystem::File _file;
  tex_detail::TexStyleContext _style_ctx;

public:
  TexProcessor();
  TexProcessor(const program_options::variables_map& args);

  std::string proc_id() const override {
    return "tex";
  }

  std::string default_output_extension() const override {
    return ".tex";
  }

  program_options::options_description program_options() const override;

  const IFoProcessor<TexProcessor>*
  lookup_fo_processor(const std::string& fo_classname) const override;

  void before_rendering() override;
  void after_rendering() override;

  bool is_verbose() const;

  fo::LengthSpec paper_width() const;
  fo::LengthSpec paper_height() const;

  std::iostream& stream() {
    return _file.stream();
  }

  tex_detail::TexStyleContext& style_ctx() {
    return _style_ctx;
  }

  void finalize_breaks();
  void request_page_break(tex_detail::BreakKind breakKind);

  void push_delayed_anchors();

  fo::LengthSpec _current_start_margin;
  int _first_page = true;
  tex_detail::BreakKind _break_pending = tex_detail::kNoBreak;
  tex_detail::CropMarksKind _cropmarks;
  // width, height, cropmarks classifier
  std::tuple<fo::LengthSpec, fo::LengthSpec, std::string> _paper_dimen;
  std::vector<std::string> _delayed_anchors;
};

} // ns eyestep
