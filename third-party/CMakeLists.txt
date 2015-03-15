# Copyright (c) 2015 Gregor Klinke
# All rights reserved.

#configure_file(<input> <output>
#               [COPYONLY] [ESCAPE_QUOTES] [@ONLY]
#               [NEWLINE_STYLE [UNIX|DOS|WIN32|LF|CRLF] ])

add_definitions(-DSEXP_USE_MODULES=1 -DSEXP_USE_BOEHM=0 -DSEXP_USE_DL=1 -DSEXP_USE_UTF8_STRINGS=1
                -DSEXP_USE_KEYWORDS=1)

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


add_library(param MODULE
  chibi-scheme/lib/srfi/39/param.c
)
target_include_directories(param PUBLIC
  "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/chibi-scheme/include>"
  chibi-scheme/include)
target_compile_options(param
  PUBLIC ${warning_options} )
target_link_libraries(param
  scheme)

set(modules_output_srfi39 "${modules_output}/srfi/39")
file(MAKE_DIRECTORY "${modules_output_srfi39}")
set_target_properties(param
  PROPERTIES PREFIX ""
             SUFFIX ".dylib"
             LIBRARY_OUTPUT_DIRECTORY ${modules_output_srfi39}
             LIBRARY_OUTPUT_DIRECTORY_DEBUG ${modules_output_srfi39}
             LIBRARY_OUTPUT_DIRECTORY_RELEASE ${modules_output_srfi39})


add_library(hash MODULE
  chibi-scheme/lib/srfi/69/hash.c
)
target_include_directories(hash PUBLIC
  "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/chibi-scheme/include>"
  chibi-scheme/include)
target_compile_options(hash
  PUBLIC ${warning_options} )
target_link_libraries(hash
  scheme)

set(modules_output_srfi69 "${modules_output}/srfi/69")
file(MAKE_DIRECTORY "${modules_output_srfi69}")
set_target_properties(hash
  PROPERTIES PREFIX ""
             SUFFIX ".dylib"
             LIBRARY_OUTPUT_DIRECTORY ${modules_output_srfi69}
             LIBRARY_OUTPUT_DIRECTORY_DEBUG ${modules_output_srfi69}
             LIBRARY_OUTPUT_DIRECTORY_RELEASE ${modules_output_srfi69})



file(COPY chibi-scheme/lib/ DESTINATION "${modules_output}"
     FILES_MATCHING REGEX ".*\\.(scm|sld)$")
