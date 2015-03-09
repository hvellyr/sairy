# Copyright (c) 2015 Gregor Klinke
# All rights reserved.

link_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/../lib
)

add_library(cppscanner
  cpp-lang.cpp
  cpp-lang.hpp
  cpp-scanner.cpp
  cpp-scanner.hpp
  nodeclass.cpp
  nodeclass.hpp
  nodelist.cpp
  nodelist.hpp
  nodes.cpp
  nodes.hpp
  source.cpp
  source.hpp
  utils.cpp
  utils.hpp
)
target_include_directories(cppscanner PUBLIC
  "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../third-party;${Boost_INCLUDE_DIRS}>")
target_compile_options(cppscanner
  PUBLIC ${cxx11_options} ${warning_options})


add_executable(sairy
  abstract-processor.ipp
  abstract-processor.hpp
  debug-processor.cpp
  debug-processor.hpp
  fo-processor.hpp
  fo.hpp
  fos.cpp
  fos.hpp
  main.cpp
  processor.hpp
  propstack.cpp
  propstack.hpp
  scanner.hpp
  scm-context.cpp
  scm-context.hpp
  sosofo.cpp
  sosofo.hpp
  style-engine.cpp
  style-engine.hpp
)
target_include_directories(sairy PUBLIC
  ../third-party/chibi-scheme/include
  "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../third-party;${Boost_INCLUDE_DIRS}>")

target_link_libraries(sairy
  cppscanner
  scheme
  clang
  ${Boost_LIBRARIES})


add_subdirectory(test)
