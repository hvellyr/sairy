# Copyright (c) 2015 Gregor Klinke
# All rights reserved.

add_library(hoedown SHARED
  ../../hoedown/src/autolink.c
  ../../hoedown/src/autolink.h
  ../../hoedown/src/buffer.c
  ../../hoedown/src/buffer.h
  ../../hoedown/src/document.c
  ../../hoedown/src/document.h
  ../../hoedown/src/escape.c
  ../../hoedown/src/escape.h
  ../../hoedown/src/html.c
  ../../hoedown/src/html.h
  ../../hoedown/src/html_blocks.c
  ../../hoedown/src/html_smartypants.c
  ../../hoedown/src/stack.c
  ../../hoedown/src/stack.h
  ../../hoedown/src/version.c
  ../../hoedown/src/version.h
)

target_include_directories(hoedown
  PUBLIC "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../hoedown/src>")
target_compile_options(hoedown
  PUBLIC ${warning_options} )
