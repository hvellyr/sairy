# Copyright (c) 2015 Gregor Klinke
# All rights reserved.

#------------------------------------------------------------------------------
add_library(sairy-utils
  utils.cpp
  utils.hpp
)
target_include_directories(sairy-utils
  PUBLIC "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}>")
target_compile_options(sairy-utils
  PUBLIC ${cxx11_options} ${warning_options})


#------------------------------------------------------------------------------
add_library(grove
  nodeclass.cpp
  nodeclass.hpp
  nodelist.cpp
  nodelist.hpp
  nodes.cpp
  nodes.hpp
  nodeutils.cpp
  nodeutils.hpp
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
add_library(textbook-parser
  textbook-model.hpp
  textbook-parser.cpp
  textbook-parser.hpp
)
target_include_directories(textbook-parser
  PUBLIC "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}>")
target_compile_options(textbook-parser
  PUBLIC ${cxx11_options} ${warning_options})


#------------------------------------------------------------------------------
add_library(textbook-scanner
  textbook-scanner.cpp
  textbook-scanner.hpp
)
target_include_directories(textbook-scanner
  PUBLIC "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}>")
target_compile_options(textbook-scanner
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
  processor-setup.cpp
  processor-setup.hpp
  scanner-setup.cpp
  scanner-setup.hpp
  scanner.hpp
  scm-context.cpp
  scm-context.hpp
  style-engine.cpp
  style-engine.hpp
)
target_include_directories(sairy
  PUBLIC "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}>;${PROJECT_SOURCE_DIR}/third-party/chibi-scheme/include")

target_link_libraries(sairy
  cppscanner
  textbook-scanner
  debug-processor
  fo
  grove
  html-processor
  processor-base
  scheme
  textbook-parser
  sairy-utils
  ${LibClang_LIBRARIES}
  ${Boost_LIBRARIES})

set(sairy_app "${PROJECT_BINARY_DIR}/app")
set_target_properties(sairy
  PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${sairy_app}")
set_target_properties(sairy
  PROPERTIES RUNTIME_OUTPUT_DIRECTORY_DEBUG "${sairy_app}")
set_target_properties(sairy
  PROPERTIES RUNTIME_OUTPUT_DIRECTORY_RELEASE "${sairy_app}")


if(POLICY CMP0026)
cmake_policy(PUSH)
cmake_policy(SET CMP0026 OLD)
endif()

get_property(sairy_executable_path TARGET sairy PROPERTY LOCATION)
configure_file("${PROJECT_SOURCE_DIR}/bin/sairy.in" "${PROJECT_SOURCE_DIR}/bin/sairy"
               @ONLY)

if(POLICY CMP0026)
cmake_policy(POP)
endif()

#------------------------------------------------------------------------------
add_subdirectory(test)
