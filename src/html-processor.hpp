// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "fo.hpp"
#include "abstract-processor.hpp"
#include "html-writer.hpp"

#include <boost/filesystem.hpp>
#include <boost/optional/optional.hpp>

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
  enum CapsStyle {
    k_normal_caps,
    k_lower_caps,
    k_upper_caps,
    k_small_caps,
  };

  using CssAttrMap = std::map<std::string, std::string>;

  struct StyleAttrs {
    StyleAttrs() : _caps(k_normal_caps) {}

    StyleAttrs(CapsStyle caps, const CssAttrMap& map)
      : _caps(caps), _css_map(map)
    {
    }

    CapsStyle _caps;
    CssAttrMap _css_map;
  };

  using RefRegistry = std::unordered_map<std::string, std::string>;
  using PortTuple = std::tuple<std::unique_ptr<html::Writer>,
                               boost::filesystem::path>;

  class HtmlRenderContext {
    std::unique_ptr<html::Writer> _port;
    boost::filesystem::path _path;
    std::list<PortTuple> _ports;
    RefRegistry _ref_registry;
    // std::list<Sosofo> _foot_notes;
    std::list<StyleAttrs> _styles_stack;

  public:
    HtmlRenderContext();

    html::Writer& port();
    boost::filesystem::path current_path();

    void push_port(std::unique_ptr<html::Writer> port,
                   const boost::filesystem::path& path);
    void pop_port();

    CapsStyle capsstyle();

    void push_styles(const StyleAttrs& map);
    void pop_styles();

    boost::optional<std::string> css_property(const std::string& key) const;
  };
} // ns detail


class HtmlProcessor : public AbstractProcessor<HtmlProcessor> {
  detail::HtmlRenderContext _ctx;

public:
  HtmlProcessor();

  std::string proc_id() const override;
  std::string default_output_extension() const override;

  const IFoProcessor<HtmlProcessor>*
  lookup_fo_processor(const std::string& fo_classname) const override;

  void before_rendering() override;
  void after_rendering() override;

  detail::HtmlRenderContext& ctx();
  html::Writer& writer();
};


} // ns eyestep
