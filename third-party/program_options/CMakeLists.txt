# Copyright (c) 2017 Gregor Klinke
# All rights reserved.

add_library(program_options STATIC
  program_options.cpp
  program_options.hpp
)

target_compile_options(program_options
  PUBLIC ${cxx11_options} ${warning_options} )
