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
    kNormalCaps,
    kUpperCaps,
    kSmallCaps,
  };

  using CssAttrMap = std::map<std::string, std::string>;

  using RefRegistry = std::unordered_map<std::string, std::string>;

  class HtmlRenderContext {
    std::unique_ptr<html::Writer> mPort;
    boost::filesystem::path mPath;
    std::list<std::tuple<std::unique_ptr<html::Writer>,
                         boost::filesystem::path>> mPorts;
    CapsStyle mCaps;
    RefRegistry mRefRegistry;
    // std::list<Sosofo> mFootNotes;
    std::list<CssAttrMap> mCssStack;

  public:
    HtmlRenderContext();

    html::Writer& port();
    boost::filesystem::path currentPath();

    void pushPort(std::unique_ptr<html::Writer> port,
                  const boost::filesystem::path& path);
    void popPort();

    CapsStyle capsStyle();
    void setCapsStyle(CapsStyle capsStyle);

    void pushCssMap(const CssAttrMap& map);
    void popCssMap();

    boost::optional<std::string> cssProperty(const std::string& key) const;
  };
} // ns detail


class HtmlProcessor : public AbstractProcessor<HtmlProcessor> {
  detail::HtmlRenderContext mCtx;

public:
  HtmlProcessor();

  std::string procId() const override;

  const IFoProcessor<HtmlProcessor>*
  lookupFoProcessor(const std::string& foClassName) const override;

  void beforeRendering() override;
  void afterRendering() override;

  detail::HtmlRenderContext& ctx();
  html::Writer& writer();
};


} // ns eyestep
