// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "cpp-lang.hpp"
#include "cpp-scanner.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"
#include "utils.hpp"

#include "clang-c/Index.h"

#include <boost/filesystem.hpp>

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


  Node* make_type_node(Grove* grove, std::string gi, Type type)
  {
    Node* nd = grove->make_node(element_class_definition());

    nd->set_property("gi", std::move(gi));
    nd->set_property("name", type.spelling());
    nd->set_property("const?", type.is_const());

    return nd;
  }


  Node* make_function_node(Grove* grove, Cursor ecursor)
  {
    Node* nd = grove->make_node(element_class_definition());

    nd->set_property("gi", "function");

    std::string nm = ecursor.spelling();

    std::vector<std::tuple<std::string, Type>> args_nm;

    visit_children(ecursor, [&args_nm](Cursor ec, Cursor ep) {
      args_nm.push_back(std::make_tuple(ec.spelling(), ec.type()));
      return CXChildVisit_Continue;
    });

    Type type = ecursor.type();
    assert(args_nm.size() == type.num_arg_types());

    nd->set_property("name", nm);
    nd->set_property(CommonProps::k_source, ecursor.location().format());

    nd->add_child_node(
      make_type_node(grove, "return-type", type.result_type()));

    auto* parameters = grove->make_node(element_class_definition());
    parameters->set_property(CommonProps::k_gi, "parameters");

    nd->add_child_node(parameters);

    for (int i = 0; i < type.num_arg_types(); i++) {
      Node* param = grove->make_node(element_class_definition());
      param->set_property(CommonProps::k_gi, "parameter");
      param->set_property("name", std::get<0>(args_nm[i]));
      param->add_child_node(
        make_type_node(grove, "type", std::get<1>(args_nm[i])));
      parameters->add_child_node(param);
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
          printf("UNHANDLED DECL TYPE %s %s (%s)", kind2str(kind), nm.c_str(),
                 loc.format().c_str());
        }
      }
      else {
        printf("SOME OTHER KIND: %s ", kind2str(kind));
      }

      printf("\n");
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

CppScanner::CppScanner(const po::variables_map& args)
  : _verbose(false)
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

  ctx._document_node->set_property(CommonProps::k_source, srcfile.string());
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
