// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "html-processor.hpp"

#include "fo-processor.hpp"
#include "fo.hpp"
#include "fos.hpp"
#include "sosofo.hpp"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>

#include <iostream>
#include <string>
#include <map>
#include <memory>


namespace eyestep {

namespace fs = boost::filesystem;


namespace {
  class HtmlLiteralFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      processor->mainStream() << static_cast<const fo::Literal*>(fo)->text();
    }
  };

  class HtmlParagraphFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      processor->mainStream() << "<p>";
      processor->renderSosofo(&fo->port("text"));
      processor->mainStream() << "</p>" << std::endl;
    }
  };

  class HtmlParagraphBreakFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      processor->mainStream() << "<br/>" << std::endl;
    }
  };

  class HtmlDisplayGroupFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      processor->mainStream() << "<div>" << std::endl;
      processor->renderSosofo(&fo->port("text"));
      processor->mainStream() << "</div>" << std::endl;
    }
  };

  class HtmlSimplePageSequenceFoProcessor : public IFoProcessor<HtmlProcessor> {
  public:
    void render(HtmlProcessor* processor,
                const IFormattingObject* fo) const override
    {
      processor->mainStream() << "<div class='page'>" << std::endl;

      processor->renderSosofo(&fo->port("text"));

      processor->mainStream() << "</div>" << std::endl;
    }
  };

} // ns anon


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

  mMainStream.open(mOutputFile.string(), std::ios_base::out |
                                             std::ios_base::binary |
                                             std::ios_base::trunc);
  if (mMainStream.fail()) {
    std::cerr << "Opening file '" << mOutputFile << "' failed" << std::endl;
  }

  writeHtmlProlog();
}


void HtmlProcessor::afterRendering()
{
  writeHtmlEpilog();
}


std::ostream& HtmlProcessor::mainStream()
{
  return mMainStream;
}


void HtmlProcessor::writeHtmlProlog()
{
  // @todo:
  std::string title;
  std::string author;
  std::string desc;

  // static const char* XHTML_1_0_TRANSITIONAL_DTD_PUBLIC_ID =
  //     "-//W3C//DTD XHTML 1.0 Transitional//EN";
  // static const char* XHTML_1_0_TRANSITIONAL_DTD_SYSTEM_ID =
  //     "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd";
  static const char* XHTML_1_1_DTD_PUBLIC_ID = "-//W3C//DTD XHTML 1.1//EN";
  static const char* XHTML_1_1_DTD_SYSTEM_ID =
      "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd";
  static const char* SAIRY_GENERATOR = "Sairy HTML Processor";

  mMainStream << "<!DOCTYPE html PUBLIC '" << XHTML_1_1_DTD_PUBLIC_ID << "' '"
              << XHTML_1_1_DTD_SYSTEM_ID << "'>" << std::endl;

  mMainStream << "<html xmlns='http://www.w3.org/1999/xhtml'>" << std::endl
              << "<head>" << std::endl
              << "<title>" << title << "</title>" << std::endl
              << "<meta http-equiv='Content-Type' content='text/html; charset=UTF-8'/>" << std::endl
              << "<meta name='generator' content='" << SAIRY_GENERATOR << "'/>" << std::endl;

  if (!author.empty()) {
    mMainStream << "<meta name='author' content='"<< author << "'/>" << std::endl;
  }
  if (!desc.empty()) {
    mMainStream << "<meta name='description' content='"<< desc << "'/>" << std::endl;
  }

  mMainStream << "</head>" << std::endl
              << "<body>" << std::endl;
}

void HtmlProcessor::writeHtmlEpilog()
{
  mMainStream << "</body>" << std::endl
              << "</html>" << std::endl;
}

} // ns eyestep
