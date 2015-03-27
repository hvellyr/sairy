# Copyright (c) 2015 Gregor Klinke
# All rights reserved.

#------------------------------------------------------------------------------
add_library(grove
  nodeclass.cpp
  nodeclass.hpp
  nodelist.cpp
  nodelist.hpp
  nodes.cpp
  nodes.hpp
)
target_include_directories(grove
  PUBLIC "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}>")
target_compile_options(grove
  PUBLIC ${cxx11_options} ${warning_options})


#------------------------------------------------------------------------------
add_library(cppscanner
  cpp-lang.cpp
  cpp-lang.hpp
  cpp-scanner.cpp
  cpp-scanner.hpp
)
target_include_directories(cppscanner
  PUBLIC "$<BUILD_INTERFACE:${LibClang_INCLUDE_DIRS};${Boost_INCLUDE_DIRS}>")
target_compile_options(cppscanner
  PUBLIC ${cxx11_options} ${warning_options})


#------------------------------------------------------------------------------
add_library(fo
  fo.hpp
  fos.cpp
  fos.hpp
  sosofo.cpp
  sosofo.hpp
)
target_include_directories(fo
  PUBLIC "$<BUILD_INTERFACE:${Boost_INCLUDE_DIRS}>")
target_compile_options(fo
  PUBLIC ${cxx11_options} ${warning_options})


#------------------------------------------------------------------------------
add_library(processor-base
  abstract-processor.hpp
  abstract-processor.ipp
  fo-processor.hpp
  processor.hpp
  propstack.cpp
  propstack.hpp
)
target_include_directories(processor-base
  PUBLIC "$<BUILD_INTERFACE:${Boost_INCLUDE_DIRS}>")
target_compile_options(processor-base
  PUBLIC ${cxx11_options} ${warning_options})


#------------------------------------------------------------------------------
add_library(html-processor
  html-processor.cpp
  html-processor.hpp
  html-writer.cpp
  html-writer.hpp
)
target_include_directories(html-processor
  PUBLIC "$<BUILD_INTERFACE:${Boost_INCLUDE_DIRS}>")
target_compile_options(html-processor
  PUBLIC ${cxx11_options} ${warning_options})


#------------------------------------------------------------------------------
add_library(debug-processor
  debug-processor.cpp
  debug-processor.hpp
)
target_include_directories(debug-processor
  PUBLIC "$<BUILD_INTERFACE:${Boost_INCLUDE_DIRS}>")
target_compile_options(debug-processor
  PUBLIC ${cxx11_options} ${warning_options})


#------------------------------------------------------------------------------
add_definitions(-DSEXP_USE_KEYWORDS=1)

add_executable(sairy
  main.cpp
  scanner.hpp
  scm-context.cpp
  scm-context.hpp
  source.cpp
  source.hpp
  style-engine.cpp
  style-engine.hpp
  utils.cpp
  utils.hpp
)
target_include_directories(sairy
  PUBLIC "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}>;${PROJECT_SOURCE_DIR}/third-party/chibi-scheme/include")

target_link_libraries(sairy
  cppscanner
  debug-processor
  fo
  grove
  html-processor
  processor-base
  scheme
  ${LibClang_LIBRARIES}
  ${Boost_LIBRARIES})


#------------------------------------------------------------------------------
add_subdirectory(test)
