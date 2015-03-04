// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "cpp-lang.hpp"
#include "cpp-scanner.hpp"
#include "nodes.hpp"
#include "nodeclass.hpp"

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

namespace {

  struct ParseContext {
    ParseContext(Grove& grove) : mGrove(grove) {}

    Grove& mGrove;
    Node* mDocumentNode;
  };


  Node* make_type_node(Grove* grove, std::string gi, Type type)
  {
    Node* nd = grove->makeNode(elementClassDefinition());

    nd->setProperty("gi", std::move(gi));
    nd->setProperty("name", type.spelling());
    nd->setProperty("const?", type.isConst());

    return nd;
  }


  Node* make_function_node(Grove* grove, Cursor ecursor)
  {
    Node* nd = grove->makeNode(elementClassDefinition());

    nd->setProperty("gi", "function");

    std::string nm = ecursor.spelling();

    std::vector<std::tuple<std::string, Type>> args_nm;

    visitChildren(ecursor, [&args_nm](Cursor ec, Cursor ep) {
      args_nm.push_back(std::make_tuple(ec.spelling(), ec.type()));
      return CXChildVisit_Continue;
    });

    Type type = ecursor.type();
    assert(args_nm.size() == type.numArgTypes());

    nd->setProperty("name", nm);
    nd->setProperty("source", ecursor.location().format());

    nd->addChildNode(make_type_node(grove, "return-type", type.resultType()));

    auto* parameters = grove->makeNode(elementClassDefinition());
    parameters->setProperty("gi", "parameters");

    nd->addChildNode(parameters);

    for (int i = 0; i < type.numArgTypes(); i++) {
      Node* param = grove->makeNode(elementClassDefinition());
      param->setProperty("gi", "parameter");
      param->setProperty("name", std::get<0>(args_nm[i]));
      param->setProperty("type", make_type_node(grove, "type",
                                                std::get<1>(args_nm[i])));
      parameters->addChildNode(param);
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
    if (loc.isFromMainFile()) {
      if (clang_isDeclaration(kind)) {
        if (kind == CXCursor_FunctionDecl) {
          Node* nd = make_function_node(ctx->mDocumentNode->grove(), ecursor);
          ctx->mDocumentNode->addChildNode(nd);

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

  int computeDataSize(const std::vector<std::string>& values, const char* opt,
                      bool isSep)
  {
    int totalsize = 0;
    int optlen = strlen(opt);

    for (const auto& str : values) {
      if (!isSep) {
        totalsize += optlen;
      }

      totalsize += str.length() + 1;
    }

    return totalsize;
  }


  int appendOptions(int in_argc, const std::vector<std::string>& values,
                    const char* opt, bool isSep, std::vector<const char*>& args,
                    std::vector<char>& argsdata)
  {
    int argc = in_argc;
    for (const auto& str : values) {
      std::string arg;
      int ofs = argsdata.size();

      if (isSep) {
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


//----------------------------------------------------------------------------------------

const std::string CppScanner::kId = "cpp";

Node* CppScanner::scanFile(eyestep::Grove& grove, const fs::path& srcfile,
                           const std::vector<std::string>& incls,
                           const std::vector<std::string>& defs)
{
  CXIndex idx;
  CXTranslationUnit tu;

  ParseContext ctx(grove);

  ctx.mDocumentNode = grove.makeNode(documentClassDefinition());

  ctx.mDocumentNode->setProperty("source", srcfile.string());
  ctx.mDocumentNode->setProperty("app-info", "cpp");

  // excludeDeclsFromPCH = 1, displayDiagnostics=1
  idx = clang_createIndex(1, 1);

  std::vector<const char*> args;
  std::vector<char> argsdata;

  int totalsize =
      computeDataSize(incls, "-I", true) + computeDataSize(incls, "-D", false);
  argsdata.resize(totalsize);

  int argc = 0;
  argc = appendOptions(argc, incls, "-I", true, args, argsdata);
  argc = appendOptions(argc, defs, "-D", false, args, argsdata);

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

  return ctx.mDocumentNode;
}

} // ns eyestep
