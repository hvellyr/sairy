// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "clang-c/Index.h"

#include <functional>
#include <iostream>
#include <sstream>
#include <string>


namespace eyestep {

class Cursor;

const char* kind2str(CXCursorKind kind);


std::string to_string(CXString cxstr);
std::string access_specifier_to_string(CX_CXXAccessSpecifier specifier);

class Type {
  CXType _type;

public:
  Type(CXType type) : _type(type) {}

  std::string spelling() const
  {
    return to_string(clang_getTypeSpelling(_type));
  }

  Type result_type() const { return Type(clang_getResultType(_type)); }

  size_t num_arg_types() const { return clang_getNumArgTypes(_type); }

  Type arg_type(size_t idx) const { return Type(clang_getArgType(_type, idx)); }

  bool is_const() const { return clang_isConstQualifiedType(_type) != 0; }

  Cursor declaration() const;

  size_t num_arg_template_arguments() const
  {
    return clang_Type_getNumTemplateArguments(_type);
  }

  Type template_arguments_as_type(size_t i)
  {
    return Type(clang_Type_getTemplateArgumentAsType(_type, i));
  }
};


class File {
  CXFile _file;

public:
  File() {}

  File(CXFile file) : _file(file) {}

  std::string filename() const { return to_string(clang_getFileName(_file)); }
};


class SourceLocation {
  CXSourceLocation _loc;
  unsigned int _line = 0;
  unsigned int _column = 0;
  unsigned int _offset = 0;
  File _file;

public:
  SourceLocation(CXSourceLocation loc) : _loc(loc)
  {
    CXFile file;
    clang_getExpansionLocation(_loc, &file, &_line, &_column, &_offset);
    _file = File(file);
  }

  std::string filename() const { return _file.filename(); }
  int line() const { return _line; }
  int column() const { return _column; }

  std::string format() const
  {
    std::stringstream ss;
    ss << _file.filename() << ":" << _line << ":" << _column;
    return ss.str();
  }

  bool is_from_main_file() const
  {
    return clang_Location_isFromMainFile(_loc) != 0;
  }
};


class TemplateArg {
  std::string _spelling;

public:
  TemplateArg(std::string spelling) : _spelling(std::move(spelling)) {}

  std::string spelling() const { return _spelling; }
};


class Cursor {
  CXCursor _cursor;

public:
  Cursor() : _cursor(clang_getNullCursor()) {}

  Cursor(CXCursor cursor) : _cursor(cursor) {}

  CXCursor cxcursor() const { return _cursor; }

  Cursor referenced_cursor() const
  {
    return Cursor(clang_getCursorReferenced(_cursor));
  }

  Cursor semantic_parent()
  {
    return Cursor(clang_getCursorSemanticParent(_cursor));
  }

  Cursor lexical_parent()
  {
    return clang_getCursorLexicalParent(_cursor);
  }

  Cursor specialized_cursor_template()
  {
    return Cursor(clang_getSpecializedCursorTemplate(_cursor));
  }

  bool is_set() const { return !clang_Cursor_isNull(_cursor); }

  CXCursorKind kind() const { return clang_getCursorKind(_cursor); }

  std::string spelling() const
  {
    return to_string(clang_getCursorSpelling(_cursor));
  }

  std::string display_name() const
  {
    return to_string(clang_getCursorDisplayName(_cursor));
  }

  SourceLocation location() const
  {
    return SourceLocation(clang_getCursorLocation(_cursor));
  }

  Type type() const { return Type(clang_getCursorType(_cursor)); }

  Type typedef_underlying_type() const
  {
    return Type(clang_getTypedefDeclUnderlyingType(_cursor));
  }

  std::string raw_comment() const
  {
    return to_string(clang_Cursor_getRawCommentText(_cursor));
  }

  CX_CXXAccessSpecifier access_specifier() const
  {
    return clang_getCXXAccessSpecifier(_cursor);
  }

  bool is_static_method() const { return clang_CXXMethod_isStatic(_cursor); }

  bool is_virtual_method() const { return clang_CXXMethod_isVirtual(_cursor); }

  bool is_pure_virtual_method() const
  {
    return clang_CXXMethod_isPureVirtual(_cursor);
  }

  bool is_const_method() const { return clang_CXXMethod_isConst(_cursor); }

  CXLinkageKind linkage() const { return clang_getCursorLinkage(_cursor); }

  bool is_class_virtual_base() const { return clang_isVirtualBase(_cursor); }

  std::string usr() const { return to_string(clang_getCursorUSR(_cursor)); }

  int num_args() const { return clang_Cursor_getNumArguments(_cursor); }

  int num_templ_args() const
  {
    return clang_Cursor_getNumTemplateArguments(_cursor);
  }

  TemplateArg template_argument(size_t idx)
  {
    auto kind = clang_Cursor_getTemplateArgumentKind(_cursor, idx);
    switch (kind) {
    case CXTemplateArgumentKind_Type:
      return TemplateArg(to_string(clang_getTypeSpelling(
        clang_Cursor_getTemplateArgumentType(_cursor, idx))));
    case CXTemplateArgumentKind_Integral: {
      std::stringstream ss;
      ss << clang_Cursor_getTemplateArgumentValue(_cursor, idx);
      return TemplateArg(ss.str());
    }
    case CXTemplateArgumentKind_NullPtr:
      return TemplateArg("nullptr");

    case CXTemplateArgumentKind_Null:
    case CXTemplateArgumentKind_Declaration:
    case CXTemplateArgumentKind_Template:
    case CXTemplateArgumentKind_TemplateExpansion:
    case CXTemplateArgumentKind_Expression:
    case CXTemplateArgumentKind_Pack:
    case CXTemplateArgumentKind_Invalid:
      return TemplateArg("???");
    }
  }
};


bool visit_children(Cursor cursor,
                    std::function<CXChildVisitResult(Cursor, Cursor)> functor);

} // ns eyestep
