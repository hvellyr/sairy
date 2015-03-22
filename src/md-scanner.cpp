// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "md-scanner.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"

#include "hoedown/src/document.h"
#include "hoedown/src/html.h"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include <algorithm>
#include <cassert>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>


namespace eyestep {

namespace fs = boost::filesystem;

namespace {
  std::string load_file_into_string(const fs::path& path)
  {
    std::stringstream result;

    fs::ifstream in(path, std::ios::in | std::ios::binary);
    in.exceptions(std::ifstream::failbit);

    result << in.rdbuf();

    return result.str();
  }


  class HoedownRenderer {
  public:
    hoedown_renderer mRenderer = {
      this,
      &blockcode,
      &blockquote,
      &header,
      &hrule,
      &list,
      &listitem,
      &paragraph,
      &table,
      &table_header,
      &table_body,
      &table_row,
      &table_cell,
      &footnotes,
      &footnote_def,
      &blockhtml,
      &autolink,
      &codespan,
      &double_emphasis,
      &emphasis,
      &underline,
      &highlight,
      &quote,
      &image,
      &linebreak,
      &link,
      &triple_emphasis,
      &strikethrough,
      &superscript,
      &footnote_ref,
      &math,
      &raw_html,
      &entity,
      &normal_text,
      &doc_header,
      &doc_footer,
    };


    static void blockcode(hoedown_buffer* ob, const hoedown_buffer* text,
                          const hoedown_buffer* lang,
                          const hoedown_renderer_data* data)
    {
      auto text_str = text ? std::string((const char*)text->data, text->size) : std::string();
      auto lang_str = lang ? std::string((const char*)lang->data, lang->size) : std::string();
      std::cout << "BLOCKCODE: [" << lang_str << "] " << std::endl
                << text_str << std::endl;
    }

    static void blockquote(hoedown_buffer* ob, const hoedown_buffer* content,
                           const hoedown_renderer_data* data)
    {
    }

    static void header(hoedown_buffer* ob, const hoedown_buffer* content,
                       int level, const hoedown_renderer_data* data)
    {
    }

    static void hrule(hoedown_buffer* ob, const hoedown_renderer_data* data)
    {
    }

    static void list(hoedown_buffer* ob, const hoedown_buffer* content,
                     hoedown_list_flags flags,
                     const hoedown_renderer_data* data)
    {
      auto content_str = content ? std::string((const char*)content->data, content->size) : std::string();
      std::cout << "LIST: [" << content_str << "]" << std::endl;
    }

    static void listitem(hoedown_buffer* ob, const hoedown_buffer* content,
                         hoedown_list_flags flags,
                         const hoedown_renderer_data* data)
    {
    }

    static void paragraph(hoedown_buffer* ob, const hoedown_buffer* content,
                          const hoedown_renderer_data* data)
    {
      auto content_str = content ? std::string((const char*)content->data, content->size) : std::string();
      std::cout << "PARAGRAPH: [" << content_str << "]" << std::endl;
    }

    static void table(hoedown_buffer* ob, const hoedown_buffer* content,
                      const hoedown_renderer_data* data)
    {
    }

    static void table_header(hoedown_buffer* ob, const hoedown_buffer* content,
                             const hoedown_renderer_data* data)
    {
    }

    static void table_body(hoedown_buffer* ob, const hoedown_buffer* content,
                           const hoedown_renderer_data* data)
    {
    }

    static void table_row(hoedown_buffer* ob, const hoedown_buffer* content,
                          const hoedown_renderer_data* data)
    {
    }

    static void table_cell(hoedown_buffer* ob, const hoedown_buffer* content,
                           hoedown_table_flags flags,
                           const hoedown_renderer_data* data)
    {
    }

    static void footnotes(hoedown_buffer* ob, const hoedown_buffer* content,
                          const hoedown_renderer_data* data)
    {
    }

    static void footnote_def(hoedown_buffer* ob, const hoedown_buffer* content,
                             unsigned int num,
                             const hoedown_renderer_data* data)
    {
    }

    static void blockhtml(hoedown_buffer* ob, const hoedown_buffer* text,
                          const hoedown_renderer_data* data)
    {
    }

    /* span level callbacks - NULL or return 0 prints the span verbatim */
    static int autolink(hoedown_buffer* ob, const hoedown_buffer* link,
                        hoedown_autolink_type type,
                        const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int codespan(hoedown_buffer* ob, const hoedown_buffer* text,
                        const hoedown_renderer_data* data)
    {
      auto text_str = text ? std::string((const char*)text->data, text->size) : std::string();
      std::cout << "CODESPAN: [" << text_str << "]" << std::endl;
      return 1;
    }

    static int double_emphasis(hoedown_buffer* ob,
                               const hoedown_buffer* content,
                               const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int emphasis(hoedown_buffer* ob, const hoedown_buffer* content,
                        const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int underline(hoedown_buffer* ob, const hoedown_buffer* content,
                         const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int highlight(hoedown_buffer* ob, const hoedown_buffer* content,
                         const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int quote(hoedown_buffer* ob, const hoedown_buffer* content,
                     const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int image(hoedown_buffer* ob, const hoedown_buffer* link,
                     const hoedown_buffer* title, const hoedown_buffer* alt,
                     const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int linebreak(hoedown_buffer* ob, const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int link(hoedown_buffer* ob, const hoedown_buffer* content,
                    const hoedown_buffer* link, const hoedown_buffer* title,
                    const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int triple_emphasis(hoedown_buffer* ob,
                               const hoedown_buffer* content,
                               const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int strikethrough(hoedown_buffer* ob, const hoedown_buffer* content,
                             const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int superscript(hoedown_buffer* ob, const hoedown_buffer* content,
                           const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int footnote_ref(hoedown_buffer* ob, unsigned int num,
                            const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int math(hoedown_buffer* ob, const hoedown_buffer* text,
                    int displaymode, const hoedown_renderer_data* data)
    {
      return 0;
    }

    static int raw_html(hoedown_buffer* ob, const hoedown_buffer* text,
                        const hoedown_renderer_data* data)
    {
      return 0;
    }

    /* low level callbacks - NULL copies input directly into the output */
    static void entity(hoedown_buffer* ob, const hoedown_buffer* text,
                       const hoedown_renderer_data* data)
    {
    }

    static void normal_text(hoedown_buffer* ob, const hoedown_buffer* text,
                            const hoedown_renderer_data* data)
    {
      if (text) {
        HOEDOWN_BUFPUTSL(ob, "@text{");
        hoedown_buffer_put(ob, text->data, text->size);
        HOEDOWN_BUFPUTSL(ob, "}");
      }
    }

    /* miscellaneous callbacks */
    static void doc_header(hoedown_buffer* ob, int inline_render,
                           const hoedown_renderer_data* data)
    {
    }

    static void doc_footer(hoedown_buffer* ob, int inline_render,
                           const hoedown_renderer_data* data)
    {
    }
  };

} // anon namespace


//----------------------------------------------------------------------------------------

MarkdownScanner::MarkdownScanner() = default;


MarkdownScanner::MarkdownScanner(
  const boost::program_options::variables_map& /*args*/)
{
}


std::string MarkdownScanner::scanner_id() const
{
  return "md";
}


std::unordered_set<std::string> MarkdownScanner::supported_extensions() const
{
  return {".md", ".markdown"};
}


boost::program_options::options_description
MarkdownScanner::program_options() const
{
  namespace po = boost::program_options;

  std::string opts_title =
    std::string("Markdown parser [selector: '") + scanner_id() + "']";
  po::options_description desc(opts_title);
  return desc;
}


Node* MarkdownScanner::scan_file(eyestep::Grove& grove, const fs::path& srcfile)
{
  Node* doc_node = grove.make_node(document_class_definition());
  doc_node->set_property("source", srcfile.string());
  doc_node->set_property("app-info", "md");

  auto text = load_file_into_string(srcfile);

  HoedownRenderer my_renderer;

  hoedown_renderer* renderer =
    hoedown_html_renderer_new(HOEDOWN_HTML_SKIP_HTML, 0);
  hoedown_document* document =
    hoedown_document_new(&my_renderer.mRenderer, hoedown_extensions(0), 16);

  hoedown_buffer* html = hoedown_buffer_new(16);
  hoedown_document_render(document, html, (const unsigned char*)text.c_str(),
                          text.length());

  std::cout << "HTML:" << std::endl
            << "--------------------------------------------------------"
            << std::endl
            << hoedown_buffer_cstr(html)
            << "--------------------------------------------------------"
            << std::endl;

  hoedown_buffer_free(html);
  hoedown_document_free(document);
  hoedown_html_renderer_free(renderer);

  return doc_node;
}

} // ns eyestep
