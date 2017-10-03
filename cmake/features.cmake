# Copyright (c) 2017 Gregor Klinke

include(CheckCXXSourceCompiles)

set(CMAKE_REQUIRED_FLAGS "${cxx11_options}")


set(CMAKE_REQUIRED_FLAGS "${cxx11_options}")
CHECK_CXX_SOURCE_COMPILES("
  #include <codecvt>
  int main() {
    std::codecvt_utf8_utf16<char16_t> x;
    return 0;
  }
" TEXTBOOK_HAVE_STD_CODECVT)


if(CMAKE_CXX_COMPILER_ID STREQUAL Clang
   OR CMAKE_CXX_COMPILER_ID STREQUAL AppleClang)
  CHECK_CXX_COMPILER_FLAG("-Wno-shift-negative-value"
    TEXTBOOK_HAVE_WARNING_SHIFT_NEGATIVE_VALUE)

  CHECK_CXX_COMPILER_FLAG("-Wno-expansion-to-defined"
    TEXTBOOK_HAVE_WARNING_EXPANSION_TO_DEFINED)


endif()