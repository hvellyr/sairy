# Copyright (c) 2015 Gregor Klinke
# All rights reserved.

#configure_file(<input> <output>
#               [COPYONLY] [ESCAPE_QUOTES] [@ONLY]
#               [NEWLINE_STYLE [UNIX|DOS|WIN32|LF|CRLF] ])

add_definitions(-DSEXP_USE_MODULES=1 -DSEXP_USE_BOEHM=0 -DSEXP_USE_DL=1)

add_library(scheme SHARED
  chibi-scheme/gc.c
  chibi-scheme/sexp.c
  chibi-scheme/bignum.c
  chibi-scheme/opcodes.c
  chibi-scheme/vm.c
  chibi-scheme/eval.c
  chibi-scheme/simplify.c
  chibi-scheme/include/chibi/sexp.h
  chibi-scheme/include/chibi/features.h
  chibi-scheme/include/chibi/install.h
  chibi-scheme/include/chibi/bignum.h
  chibi-scheme/include/chibi/eval.h
)

target_include_directories(scheme PUBLIC
  "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/chibi-scheme/include>"
  chibi-scheme/include)
target_compile_options(scheme
  PUBLIC ${warning_options} )


set(modules_output "${PROJECT_BINARY_DIR}/scheme/lib")
file(COPY chibi-scheme/lib/ DESTINATION "${modules_output}"
     FILES_MATCHING REGEX ".*\\.(scm|sld)$")
