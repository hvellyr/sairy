// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "cpp-lang.hpp"
#include "cpp-scanner.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"
#include "utils.hpp"
#include "cpp-comments.hpp"
#include "textbook-parser.hpp"
#include "textbook-model.hpp"

#include "clang-c/Index.h"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/range/adaptor/transformed.hpp>

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>


namespace eyestep {

namespace fs = boost::filesystem;
namespace po = boost::program_options;


namespace {

  struct ParseContext {
    ParseContext(Grove& grove) : _grove(grove) {}

    Grove& _grove;
    Node* _document_node;
  };

  enum Kind {
    k_function = 0,
  };

  std::string make_id(const std::string& dialect, Kind kind,
                      const std::string& nm,
                      const std::string& args = "",
                      const std::string& annotation = "")
  {
    static const auto kind2nm = std::vector<std::string>{"function"};

    std::stringstream ss;
    ss << dialect << "/" << kind2nm[kind] << "/";
    ss << nm;

    if (kind == k_function) {
      ss << "(" << args << ")";
    }
    if (!annotation.empty()) {
      ss << "/" << annotation;
    }
    return ss.str();
  }


  std::string path_rel_to_cwd(const SourceLocation& loc)
  {
    std::stringstream ss;
    ss << utils::make_relative(fs::current_path(), loc.filename()).string()
       << ":" << loc.line() << ":" << loc.column();
    return ss.str();
  }


  Node* make_type_node(Grove* grove, const std::string& gi, Type type)
  {
    Node* nd = grove->make_elt_node(gi);

    nd->add_attribute("name", type.spelling());
    nd->add_attribute("const?", type.is_const());

    return nd;
  }


  Node* make_desc_node(ParseContext* ctx, Grove* grove, const Cursor& ecursor)
  {
    Node* desc_node = nullptr;

    auto comment = ecursor.raw_comment();
    auto norm_comment = normalize_comment(comment);
    if (boost::starts_with(norm_comment, "@doc")) {
      auto comment2 = norm_comment + "\n@end doc";

      Node* doc_node = grove->make_elt_node("doc");
      textbook::GroveBuilder grove_builder(doc_node);
      textbook::VariableEnv vars;
      textbook::Catalog catalog;
      textbook::Parser parser(*grove, grove_builder, vars, catalog,
                              nullptr, // docspec
                              {fs::path("share/sairy/textbook/spec")},
                              false, // mixed content
                              false  // verbose
                              );

      parser.parse_string(comment2);

      auto nodes = doc_node->property<Nodes>(CommonProps::k_children);
      if (!nodes.empty()) {
        auto grand_children =
          nodes[0]->property<Nodes>(CommonProps::k_children);
        unparent_nodes(grand_children);

        desc_node = grove->make_elt_node("desc");
        for (auto* docnd : grand_children) {
          desc_node->add_child_node(docnd);
        }
      }

      grove->remove_node(doc_node);
    }

    return desc_node;
  }


  /*! results in:
   *
   * @tag{function}    {name}          {ANY}
   * @tag{parameters}                  {ANY}
   * @tag{parameter}   {name, const?}  {ANY}
   * @tag{return-type} {name, const?}  {ANY}
   * @tag{type}        {name, const?}  {ANY}
   * @tag{desc}        {}              {TEXT|ANY}
   */
  Node* make_function_node(ParseContext* ctx, Grove* grove, Cursor ecursor)
  {
    Node* nd = grove->make_elt_node("function");
    nd->set_property(CommonProps::k_source,
                     path_rel_to_cwd(ecursor.location()));

    std::string nm = ecursor.spelling();

    std::vector<std::tuple<std::string, Type>> args_nm;

    visit_children(ecursor, [&args_nm](Cursor ec, Cursor ep) {
      args_nm.push_back(std::make_tuple(ec.spelling(), ec.type()));
      return CXChildVisit_Continue;
    });

    Type type = ecursor.type();
    assert(args_nm.size() == type.num_arg_types());

    nd->add_attribute("name", nm);
    nd->add_attribute("dialect", "cpp");

    nd->set_property(CommonProps::k_id,
                     make_id("cpp", k_function, nm,
                             utils::join(boost::copy_range<
                                           std::vector<std::string>>(
                                           args_nm |
                                           boost::adaptors::transformed([](
                                             const std::tuple<std::string,
                                                              Type>& tup) {
                                             return std::get<1>(tup).spelling();
                                           })),
                                         ","),
                             ""));

    nd->add_child_node(
      make_type_node(grove, "return-type", type.result_type()));

    auto* parameters = grove->make_elt_node("parameters");
    nd->add_child_node(parameters);

    for (int i = 0; i < type.num_arg_types(); i++) {
      Node* param = grove->make_elt_node("parameter");
      param->add_attribute("name", std::get<0>(args_nm[i]));
      param->add_child_node(
        make_type_node(grove, "type", std::get<1>(args_nm[i])));
      parameters->add_child_node(param);
    }

    auto* desc_node = make_desc_node(ctx, grove, ecursor);
    if (desc_node) {
      nd->add_child_node(desc_node);
    }

    return nd;
  }


  CXChildVisitResult scan_hierarchy_visitor(CXCursor cursor, CXCursor parent,
                                            CXClientData client_data)
  {
    Cursor ecursor(cursor);

    CXChildVisitResult retval = CXChildVisit_Continue;

    ParseContext* ctx = static_cast<ParseContext*>(client_data);
    CXCursorKind kind = ecursor.kind();

    std::string nm = ecursor.spelling();

    SourceLocation loc = ecursor.location();
    if (loc.is_from_main_file()) {
      if (clang_isDeclaration(kind)) {
        if (kind == CXCursor_FunctionDecl) {
          Node* nd = make_function_node(ctx->_document_node->grove(), ecursor);
          ctx->_document_node->add_child_node(nd);

          retval = CXChildVisit_Recurse;
        }
        else {
          printf("UNHANDLED DECL TYPE %s %s (%s)\n", kind2str(kind), nm.c_str(),
                 loc.format().c_str());
        }
      }
      else {
        printf("SOME OTHER KIND: %s \n", kind2str(kind));
      }
    }
    return retval;
  }

} // end anon namespace


//----------------------------------------------------------------------------------------

namespace {

  int compute_data_size(const std::vector<std::string>& values, const char* opt,
                        bool is_sep)
  {
    int totalsize = 0;
    int optlen = strlen(opt);

    for (const auto& str : values) {
      if (!is_sep) {
        totalsize += optlen;
      }

      totalsize += str.length() + 1;
    }

    return totalsize;
  }


  int append_options(int in_argc, const std::vector<std::string>& values,
                     const char* opt, bool is_sep,
                     std::vector<const char*>& args,
                     std::vector<char>& argsdata)
  {
    int argc = in_argc;
    for (const auto& str : values) {
      std::string arg;
      int ofs = argsdata.size();

      if (is_sep) {
        args.push_back(opt);
        argc++;

        arg = str;
      }
      else {
        arg = std::string() + opt + str;
      }

      argsdata.reserve(arg.length() + 1);
      argsdata.insert(argsdata.end(), arg.begin(), arg.end());
      argsdata.push_back(0);

      args.push_back(&argsdata[ofs]);
      argc++;
    }

    return argc;
  }

} // ns anon


//------------------------------------------------------------------------------

CppScanner::CppScanner() : _verbose(false)
{
}

CppScanner::CppScanner(const po::variables_map& args) : _verbose(false)
{
  if (!args.empty()) {

    std::vector<std::string> incl_paths;
    if (args.count("include-path")) {
      incl_paths = args["include-path"].as<std::vector<std::string>>();
    }

    if (args.count("isystem")) {
      auto v = args["isystem"].as<std::vector<std::string>>();
      incl_paths.insert(incl_paths.end(), v.begin(), v.end());
    }

    std::vector<std::string> defs;
    if (args.count("defs")) {
      defs = args["defs"].as<std::vector<std::string>>();
    }

    _incl_paths.swap(incl_paths);
    _defs.swap(defs);

    _verbose = args["verbose"].as<bool>();
    if (_verbose) {
      std::cout << "Cpp scanner:" << std::endl
                << "incl paths  : " << eyestep::utils::join(incl_paths, " ")
                << std::endl
                << "defs        : " << eyestep::utils::join(defs, " ")
                << std::endl;
    }
  }
}

std::string CppScanner::scanner_id() const
{
  return "cpp";
}

std::unordered_set<std::string> CppScanner::supported_extensions() const
{
  return {".c",   ".cpp", ".cxx", ".h", ".hh",
          ".hpp", ".hxx", ".ipp", ".m", ".mm"};
}


po::options_description CppScanner::program_options() const
{
  std::string opts_title =
    std::string("C++ parser [selector: '") + scanner_id() + "']";
  po::options_description desc(opts_title);

  // clang-format off
  desc.add_options()
    ("include-path,I", po::value<std::vector<std::string>>()->composing(),
                       "add include path to C parser")
    ("defs,D",         po::value<std::vector<std::string>>()->composing(),
                       "add preprocessor defines")
    ("isysroot",       po::value<std::vector<std::string>>()->composing(),
                       "ignored")
    ("isystem",        po::value<std::vector<std::string>>()->composing(),
                       "-isystem arg, like -I")
    ;
  // clang-format on

  return desc;
}

Node* CppScanner::scan_file(eyestep::Grove& grove, const fs::path& srcfile)
{
  CXIndex idx;
  CXTranslationUnit tu;

  ParseContext ctx(grove);

  ctx._document_node = grove.make_node(document_class_definition());

  ctx._document_node->set_property(CommonProps::k_source,
                                   utils::make_relative(fs::current_path(),
                                                        srcfile).string());
  ctx._document_node->set_property("app-info", "cpp");

  // excludeDeclsFromPCH = 1, displayDiagnostics=1
  idx = clang_createIndex(1, 1);

  std::vector<const char*> args;
  std::vector<char> argsdata;

  int totalsize = compute_data_size(_incl_paths, "-I", true) +
                  compute_data_size(_defs, "-D", false);
  argsdata.resize(totalsize);

  int argc = 0;
  argc = append_options(argc, _incl_paths, "-I", true, args, argsdata);
  argc = append_options(argc, _defs, "-D", false, args, argsdata);

  args.push_back("-x");
  argc++;
  args.push_back("c++");
  argc++;

  unsigned int options = CXTranslationUnit_SkipFunctionBodies |
                         CXTranslationUnit_DetailedPreprocessingRecord |
                         CXTranslationUnit_Incomplete |
                         CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;


  tu = clang_parseTranslationUnit(idx, srcfile.string().c_str(),
                                  (const char* const*)&args[0], argc,
                                  (struct CXUnsavedFile*)NULL,
                                  0, // num_unsaved_files,
                                  options);

  clang_visitChildren(clang_getTranslationUnitCursor(tu),
                      (CXCursorVisitor)scan_hierarchy_visitor, &ctx);
  clang_disposeTranslationUnit(tu);

  return ctx._document_node;
}

} // ns eyestep
