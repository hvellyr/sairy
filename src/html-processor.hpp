// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "abstract-processor.hpp"
#include "fo.hpp"
#include "html-types.hpp"
#include "html-writer.hpp"

#include "cxxopts.hpp"
#include "fspp/estd/optional.hpp"
#include "fspp/filesystem.hpp"

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


namespace detail {
  enum CapsStyle
  {
    k_normal_caps,
    k_lower_caps,
    k_upper_caps,
    k_small_caps,
  };


  using CssAttrMap = std::map<std::string, std::string>;

  struct StyleAttrs
  {
    StyleAttrs()
      : _caps(k_normal_caps) {}

    StyleAttrs(CapsStyle caps, const CssAttrMap& map)
      : _caps(caps)
      , _css_map(map) {}

    CapsStyle _caps;
    CssAttrMap _css_map;
  };


  using RefRegistry = std::unordered_map<std::string, std::string>;
  using PortTuple = std::tuple<std::unique_ptr<html::Writer>, filesystem::path>;

  class HtmlRenderContext
  {
    std::unique_ptr<html::Writer> _port;
    filesystem::path _path;
    std::list<PortTuple> _ports;
    RefRegistry _ref_registry;
    std::list<StyleAttrs> _styles_stack;

  public:
    HtmlRenderContext() = default;

    html::Writer& port();
    filesystem::path current_path();

    void push_port(std::unique_ptr<html::Writer> port, const filesystem::path& path);
    void pop_port();

    CapsStyle capsstyle();

    void push_styles(const StyleAttrs& map);
    void pop_styles();

    estd::optional<std::string> css_property(const std::string& key) const;
  };
} // ns detail


class HtmlProcessor : public AbstractProcessor<HtmlProcessor>
{
  detail::HtmlRenderContext _ctx;
  html::detail::StyleCtx _style_ctx;
  bool _verbose;
  html::CSSWriter _css_port;
  filesystem::path _css_file;
  estd::optional<fo::LengthSpec> _top_zone_offset;

public:
  HtmlProcessor();
  HtmlProcessor(const cxxopts::ParseResult& args);

  std::string proc_id() const override {
    return "html";
  }

  std::string default_output_extension() const override {
    return ".html";
  }

  void add_program_options(cxxopts::Options& options) const override;

  const IFoProcessor<HtmlProcessor>*
  lookup_fo_processor(const std::string& fo_classname) const override;

  void before_rendering() override;
  void after_rendering() override;

  detail::HtmlRenderContext& ctx() {
    return _ctx;
  }

  html::detail::StyleCtx& style_ctx() {
    return _style_ctx;
  }

  html::Writer& writer() {
    return _ctx.port();
  }

  html::CSSWriter& css_writer() {
    return _css_port;
  }

  bool is_verbose() const {
    return _verbose;
  }

  filesystem::path css_file() const {
    return _css_file;
  }

  void set_top_zone_offset(const fo::LengthSpec& offset) {
    _top_zone_offset = offset;
  }

  estd::optional<fo::LengthSpec> top_zone_offset() const {
    return _top_zone_offset;
  }
};

} // ns eyestep
