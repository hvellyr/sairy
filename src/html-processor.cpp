// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "html-processor.hpp"

#include "fo-processor.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"

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

namespace {
  class HtmlLiteralFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      auto str = static_cast<const fo::Literal*>(fo)->text();
      std::cout << "#literal[html-processor] value: '" << str << "'" << std::endl;
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
      html::Tag with_tag(processor->writer(), "div", { html::Attr{"class", "page"} });

      processor->renderSosofo(&fo->port("text"));
    }
  };

} // ns anon


HtmlProcessor::HtmlProcessor() : mWriter(html::kXHTML_1_1_DTD, kSairyGenerator)
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
  std::cout << "DEBUG: Processor: " << procId() << std::endl;

  mWriter.open(mOutputFile);

  writeHtmlProlog();
}


void HtmlProcessor::afterRendering()
{
  writeHtmlEpilog();
}


html::Writer& HtmlProcessor::writer()
{
  return mWriter;
}


void HtmlProcessor::writeHtmlProlog()
{
  // @todo:
  std::string title;
  std::string author;
  std::string desc;

  mWriter.header(title, author, desc, [](std::ostream&) { });
}

void HtmlProcessor::writeHtmlEpilog()
{
  mWriter.footer();
}

} // ns eyestep
