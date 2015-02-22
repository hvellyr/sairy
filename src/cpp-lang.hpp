// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

#include "clang-c/Index.h"

#include <string>
#include <iostream>
#include <sstream>

namespace eyestep {

const char* kind2str(enum CXCursorKind kind);

std::string toString(CXString cxstr);

class Type {
  CXType mType;

public:
  Type(CXType type) : mType(type) {}

  std::string spelling() const
  {
    return toString(clang_getTypeSpelling(mType));
  }

  Type resultType() const { return Type(clang_getResultType(mType)); }

  size_t numArgTypes() const { return clang_getNumArgTypes(mType); }

  Type argType(size_t idx) const { return Type(clang_getArgType(mType, idx)); }

  bool isConst() const { return clang_isConstQualifiedType(mType) != 0; }
};


class File {
  CXFile mFile;

public:
  File() {}

  File(CXFile file) : mFile(file) {}

  std::string fileName() const { return toString(clang_getFileName(mFile)); }
};


class SourceLocation {
  CXSourceLocation mLoc;
  unsigned int mLine = 0;
  unsigned int mColumn = 0;
  unsigned int mOffset = 0;
  File mFile;

public:
  SourceLocation(CXSourceLocation loc) : mLoc(loc)
  {
    CXFile file;
    clang_getExpansionLocation(mLoc, &file, &mLine, &mColumn, &mOffset);
    mFile = File(file);
  }

  std::string fileName() const { return mFile.fileName(); }

  std::string format() const
  {
    std::stringstream ss;
    ss << mFile.fileName() << ":" << mLine << ":" << mColumn;
    return ss.str();
  }

  bool isFromMainFile() const
  {
    return clang_Location_isFromMainFile(mLoc) != 0;
  }
};


class Cursor {
  CXCursor mCursor;

public:
  Cursor(CXCursor cursor) : mCursor(cursor) {}

  CXCursor cxcursor() const { return mCursor; }

  CXCursorKind kind() const { return clang_getCursorKind(mCursor); }

  std::string spelling() const
  {
    return toString(clang_getCursorSpelling(mCursor));
  }

  SourceLocation location() const
  {
    return SourceLocation(clang_getCursorLocation(mCursor));
  }

  Type type() const { return Type(clang_getCursorType(mCursor)); }
};


bool visitChildren(Cursor cursor,
                   std::function<CXChildVisitResult(Cursor, Cursor)> functor);

} // ns eyestep
