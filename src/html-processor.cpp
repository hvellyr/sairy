// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "html-processor.hpp"

#include "fo-processor.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"
#include "estd/memory.hpp"

#include <boost/filesystem.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>

#include <iostream>
#include <string>
#include <map>
#include <memory>


namespace eyestep {

namespace fs = boost::filesystem;

const std::string kSairyGenerator = "Sairy HTML Processor";

detail::HtmlRenderContext::HtmlRenderContext() : mCaps(detail::kNormalCaps)
{
}

html::Writer& detail::HtmlRenderContext::port()
{
  return *mPort.get();
}

boost::filesystem::path detail::HtmlRenderContext::currentPath()
{
  return mPath;
}

void detail::HtmlRenderContext::pushPort(std::unique_ptr<html::Writer> port,
                                         const fs::path& path)
{
  mPorts.push_front(
      std::tuple<std::unique_ptr<html::Writer>, fs::path>(std::move(mPort),
                                                          mPath));
  mPort = std::move(port);
  mPath = path;
}

void detail::HtmlRenderContext::popPort()
{
  if (!mPorts.empty()) {
    auto tup = std::move(mPorts.front());
    mPort = std::move(std::get<0>(tup));
    mPath = std::get<1>(tup);
    mPorts.pop_front();
  }
}


namespace {
  template <typename Functor>
  void withHtmlFile(HtmlProcessor* processor, const fs::path& path,
                    const std::string& title, const std::string& author,
                    const std::string& desc, const html::Doctype& doctype,
                    Functor functor)
  {
    detail::HtmlRenderContext& ctx = processor->ctx();

    auto port = estd::make_unique<html::Writer>(doctype, kSairyGenerator);
    port->open(path);

    ctx.pushPort(std::move(port), path);

    ctx.port().header(title, author, desc, [](std::ostream&) {});

    functor(processor);

    ctx.port().footer();
    ctx.popPort();
  }


  class HtmlLiteralFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      auto str = static_cast<const fo::Literal*>(fo)->text();
      std::cout << "#literal[html-processor] value: '" << str << "'"
                << std::endl;
      processor->writer().write_text(str);
    }
  };

  class HtmlParagraphFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      html::Tag with_tag(processor->writer(), "p");
      processor->renderSosofo(&fo->port("text"));
    }
  };

  class HtmlParagraphBreakFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      processor->writer().empty_tag("br");
    }
  };

  class HtmlDisplayGroupFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      html::Tag with_tag(processor->writer(), "div");
      processor->renderSosofo(&fo->port("text"));
    }
  };

  class HtmlSimplePageSequenceFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      auto title = processor->property(fo, "metadata:title", std::string());
      auto author = processor->property(fo, "metadata:author", std::string());
      auto desc = processor->property(fo, "metadata:desc", std::string());

      withHtmlFile(processor, processor->ctx().currentPath(), title, author,
                   desc,
                   html::kXHTML_1_1_DTD,
                   [fo](HtmlProcessor* processor) {
                     html::Tag with_tag(processor->ctx().port(), "div",
                                        {html::Attr{"class", "page"}});

                     processor->renderSosofo(&fo->port("text"));
                   });
    }
  };

} // ns anon


HtmlProcessor::HtmlProcessor()
{
}


std::string HtmlProcessor::procId() const
{
  return "#html-processor";
}


const IFoProcessor<HtmlProcessor>*
HtmlProcessor::lookupFoProcessor(const std::string& foClassName) const
{
  static auto procs =
      std::map<std::string, std::shared_ptr<IFoProcessor<HtmlProcessor>>>{
          {"#literal", std::make_shared<HtmlLiteralFoProcessor>()},
          {"#paragraph", std::make_shared<HtmlParagraphFoProcessor>()},
          {"#paragraph-break",
           std::make_shared<HtmlParagraphBreakFoProcessor>()},
          {"#display-group", std::make_shared<HtmlDisplayGroupFoProcessor>()},
          {"#simple-page-sequence",
           std::make_shared<HtmlSimplePageSequenceFoProcessor>()},
      };

  auto i_find = procs.find(foClassName);

  return i_find != procs.end() ? i_find->second.get() : nullptr;
}


void HtmlProcessor::beforeRendering()
{
  auto mainPort =
      estd::make_unique<html::Writer>(html::kXHTML_1_1_DTD, kSairyGenerator);
  mCtx.pushPort(std::move(mainPort), mOutputFile);
}


void HtmlProcessor::afterRendering()
{
}


detail::HtmlRenderContext& HtmlProcessor::ctx()
{
  return mCtx;
}


html::Writer& HtmlProcessor::writer()
{
  return mCtx.port();
}

} // ns eyestep
