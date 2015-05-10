// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "clang-c/Index.h"

#include <functional>
#include <iostream>
#include <sstream>
#include <string>


namespace eyestep {

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


class Cursor {
  CXCursor _cursor;

public:
  Cursor(CXCursor cursor) : _cursor(cursor) {}

  CXCursor cxcursor() const { return _cursor; }

  CXCursorKind kind() const { return clang_getCursorKind(_cursor); }

  std::string spelling() const
  {
    return to_string(clang_getCursorSpelling(_cursor));
  }

  SourceLocation location() const
  {
    return SourceLocation(clang_getCursorLocation(_cursor));
  }

  Type type() const { return Type(clang_getCursorType(_cursor)); }

  std::string raw_comment() const
  {
    return to_string(clang_Cursor_getRawCommentText(_cursor));
  }

  CX_CXXAccessSpecifier access_specifier() const
  {
    return clang_getCXXAccessSpecifier(_cursor);
  }

  bool is_static() const
  {
    return clang_CXXMethod_isStatic(_cursor);
  }

  CXLinkageKind linkage() const
  {
    return clang_getCursorLinkage(_cursor);
  }
};


bool visit_children(Cursor cursor,
                    std::function<CXChildVisitResult(Cursor, Cursor)> functor);

} // ns eyestep
