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

  hoedown_renderer* renderer =
    hoedown_html_renderer_new(HOEDOWN_HTML_SKIP_HTML, 0);
  hoedown_document* document =
    hoedown_document_new(renderer, hoedown_extensions(0), 16);

  hoedown_buffer* html = hoedown_buffer_new(16);
  hoedown_document_render(document, html,
                          (const unsigned char*)text.c_str(),
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
