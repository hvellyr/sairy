# Copyright (c) 2015 Gregor Klinke
# All rights reserved.

link_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/../lib
)

add_library(cppscanner
  cpp-lang.hpp
  cpp-lang.cpp
  cpp-scanner.hpp
  cpp-scanner.cpp
  nodes.hpp
  nodes.cpp
  source.hpp
  source.cpp
  utils.hpp
  utils.cpp
)
target_include_directories(cppscanner PUBLIC
  "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../include;${Boost_INCLUDE_DIRS}>")
target_compile_options(cppscanner
  PUBLIC ${cxx11_options} ${warning_options})


add_executable(sairy
  main.cpp
)

target_link_libraries(sairy
  cppscanner
  clang
  ${Boost_LIBRARIES})


add_subdirectory(test)
