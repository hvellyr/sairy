# Copyright (c) 2017 Gregor Klinke
# All rights reserved.

if(TEXTBOOK_HAVE_WARNING_SHIFT_NEGATIVE_VALUE)
  set(add_warning_options ${add_warning_options} -Wno-shift-negative-value)
endif()
if(TEXTBOOK_HAVE_WARNING_EXPANSION_TO_DEFINED)
  set(add_warning_options ${add_warning_options} -Wno-expansion-to-defined)
endif()

add_definitions(-DSEXP_USE_MODULES=1
                -DSEXP_USE_BOEHM=0
                -DSEXP_USE_UTF8_STRINGS=1
                -DSEXP_USE_KEYWORDS=1
                -DSEXP_USE_QUANTITY=1
                -DSEXP_INITIAL_HEAP_SIZE=42*1024*1024
                -DSEXP_USE_STATIC_LIBS=1
                -DSEXP_USE_STATIC_LIBS_NO_INCLUDE=1)

add_library(scheme STATIC
  clibs.c
  ../../chibi-scheme/gc.c
  ../../chibi-scheme/sexp.c
  ../../chibi-scheme/bignum.c
  ../../chibi-scheme/opcodes.c
  ../../chibi-scheme/vm.c
  ../../chibi-scheme/eval.c
  ../../chibi-scheme/simplify.c
  ../../chibi-scheme/include/chibi/sexp.h
  ../../chibi-scheme/include/chibi/features.h
  ../../chibi-scheme/include/chibi/install.h
  ../../chibi-scheme/include/chibi/bignum.h
  ../../chibi-scheme/include/chibi/eval.h
)

target_include_directories(scheme PUBLIC
  "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../chibi-scheme/include>"
  ../../chibi-scheme/include)
target_compile_options(scheme
  PUBLIC ${warning_options} ${add_warning_options} )


