# Based on a PR from Damien Buhl 2013 (https://github.com/Kitware/CMake/pull/34)
# Adaption by hvellyr 2015
#
# - Find libclang
# Find the native LibClang includes and library.
# Once done this will define
#
#  LibClang_INCLUDE_DIRS - where to find clang/ and clang-c/ includes
#  LibClang_LIBRARIES    - List of libraries to link to libclang
#
#  LibClang_VERSION        - The version of libclang found (x.y)
#  LibClang_VERSION_MAJOR  - The major version of libclang
#  LibClang_VERSION_MINOR  - The minor version of libclang
#
# An includer may set LibClang_ROOT to a clang installation root to tell
# this module where to look.
#
# Usage examples
#   find_package(LibClang REQUIRED)
#   find_package(LibClang 3.2)

include(FindPackageHandleStandardArgs)

find_path(LibClang_ROOT_DIR
  NAMES include/clang include/clang-c
  PATHS /usr/lib/llvm-3.5 /usr/local/lib/llvm-3.5 /opt/local/lib/llvm-3.5
        /usr /usr/local /opt/local
    ENV LibClang_ROOT_DIR
  NO_DEFAULT_PATH
  DOC "libclang root directory")

find_path(LibClang_INCLUDE_DIR
    NAMES clang/Analysis clang/ARCMigrate
    lang/AST clang/ASTMatchers clang/Basic
    clang/CodeGen clang/Config clang/Driver
    clang/Edit clang/Frontend clang/FrontendTool
    clang/Lex clang/Parse clang/Rewrite clang/Sema
    clang/Serialization clang/StaticAnalyzer clang/Tooling
    clang-c/Index.h
    HINTS ${LibClang_ROOT_DIR}
    PATH_SUFFIXES include
    DOC "libclang include directory")

if (LibClang_INCLUDE_DIR)
    set (_LibClang_VERSION_HEADER ${LibClang_INCLUDE_DIR}/clang/Basic/Version.inc)
    if (EXISTS ${_LibClang_VERSION_HEADER})
        file (STRINGS ${_LibClang_VERSION_HEADER} _LibClang_VERSION_CONTENT)

        string (REGEX REPLACE
            ".*#define CLANG_VERSION_MAJOR[ \t]+([0-9]+).*" "\\1"
            LibClang_VERSION_MAJOR ${_LibClang_VERSION_CONTENT})

        string (REGEX REPLACE
            ".*#define CLANG_VERSION_MINOR[ \t]+([0-9]+).*" "\\1"
            LibClang_VERSION_MINOR ${_LibClang_VERSION_CONTENT})

        SET (LibClang_VERSION ${LibClang_VERSION_MAJOR}.${LibClang_VERSION_MINOR})

    endif (EXISTS ${_LibClang_VERSION_HEADER})
endif (LibClang_INCLUDE_DIR)


find_library(LibClang_LIBRARY
  NAMES clang clang-${LibClang_VERSION}
  HINTS ${LibClang_ROOT_DIR}
  PATH_SUFFIXES lib
  "libclang library")

set(LibClang_LIBRARIES ${LibClang_LIBRARY})
set(LibClang_INCLUDE_DIRS ${LibClang_INCLUDE_DIR})

mark_as_advanced(LibClang_ROOT_DIR LibClang_INCLUDE_DIR LibClang_LIBRARY)

find_package_handle_standard_args(LibClang
  REQUIRED_VARS LibClang_ROOT_DIR LibClang_LIBRARY LibClang_INCLUDE_DIR
  VERSION_VAR LibClang_VERSION)
