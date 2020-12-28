// Copyright (c) 2020 Gregor Klinke

#pragma once

#include "config.hpp"

// clang-format off

#if defined(__clang__)

  #define TEXTBOOK_SUPPRESS_WARNINGS \
    _Pragma("clang diagnostic push") \
    _Pragma("clang diagnostic ignored \"-Wsign-compare\"") \
    /**/

  #define TEXTBOOK_RESTORE_WARNINGS \
    _Pragma("clang diagnostic pop")

  #define TEXTBOOK_SUPPRESS_SIGNCOMPARE_WARNING
    _Pragma("clang diagnostic push") \
    _Pragma("clang diagnostic ignored \"-Wsign-compare\"") \
    /**/

#elif defined(_MSC_VER)

  #define TEXTBOOK_SUPPRESS_WARNINGS \
    __pragma(warning(push, 0)) \
    __pragma(warning(disable: 4244))

  #define TEXTBOOK_RESTORE_WARNINGS \
    __pragma(warning(pop))

  #define TEXTBOOK_SUPPRESS_SIGNCOMPARE_WARNING

#elif defined(__GNUC__)

  #if defined(TEXTBOOK_HAVE_MISLEADING_INDENTATION_WARNING)
    #define TEXTBOOK_IGNORE_MISLEADING_INDENTATION \
    _Pragma("GCC diagnostic ignored \"-Wmisleading-indentation\"")
  #else
    #define TEXTBOOK_IGNORE_MISLEADING_INDENTATION
  #endif

  #define TEXTBOOK_SUPPRESS_WARNINGS \
    _Pragma("GCC diagnostic push") \
    _Pragma("GCC diagnostic ignored \"-Wsign-compare\"") \
    TEXTBOOK_IGNORE_MISLEADING_INDENTATION \
    /**/

  #define TEXTBOOK_RESTORE_WARNINGS _Pragma("GCC diagnostic pop")

  #define TEXTBOOK_SUPPRESS_SIGNCOMPARE_WARNING \
    _Pragma("GCC diagnostic push") \
    _Pragma("GCC diagnostic ignored \"-Wsign-compare\"") \
    /**/

#else

  #define TEXTBOOK_SUPPRESS_WARNINGS
  #define TEXTBOOK_RESTORE_WARNINGS

  #define TEXTBOOK_SUPPRESS_SIGNCOMPARE_WARNING

#endif

// clang-format on
