# Copyright (c) 2017 Gregor Klinke
# All rights reserved.

cmake_minimum_required(VERSION 2.8.12)

project(Textbook C CXX)
set(Textbook_VERSION_MAJOR 0)
set(Textbook_VERSION_MINOR 1)

# Enable C++11
include(CheckCXXCompilerFlag)
if(WIN32)
  # Check if we are using Visual Studio 2013 or later
  if(CMAKE_CXX_COMPILER_VERSION VERSION_LESS 12)
    message(FATAL_ERROR "You are using an unsupported Windows compiler! (Visual C++ 2013 or later required)")
  endif()
else()
  set(cxx11_options -std=c++11)
  CHECK_CXX_COMPILER_FLAG(${cxx11_options} COMPILER_SUPPORTS_CXX11)

  if(NOT COMPILER_SUPPORTS_CXX11)
    message(FATAL_ERROR "The compiler ${CMAKE_CXX_COMPILER} has insufficient C++11 support. Please use a different C++ compiler.")
  endif()
endif()

# Build debug by default
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'Debug' as none was specified.")
  set(CMAKE_BUILD_TYPE Debug)
endif()

include(cmake/features.cmake)

# Enable warnings
if(CMAKE_CXX_COMPILER_ID STREQUAL Clang
    OR CMAKE_CXX_COMPILER_ID STREQUAL AppleClang)
  set(warning_options
    -Wall
  )
elseif(MSVC)
  set(warning_options /WX /W4 /wd4503 /wd4512)
endif()

find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "${CCACHE_PROGRAM}")
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK "${CCACHE_PROGRAM}")
endif()

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_INCLUDE_CURRENT_DIR_IN_INTERFACE ON)
set(CMAKE_MACOSX_RPATH ON)

CONFIGURE_FILE(${PROJECT_SOURCE_DIR}/src/config.hpp.in ${PROJECT_BINARY_DIR}/src/config.hpp)

enable_testing()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

include(FeatureSummary)
feature_summary(WHAT ALL FATAL_ON_MISSING_REQUIRED_PACKAGES)

add_subdirectory(third-party)
add_subdirectory(src)
