# Copyright (c) 2017 Gregor Klinke
# All rights reserved.

set(fspp_LIBRARIES "${PROJECT_BINARY_DIR}/lib/libfspplib.a")
set(include_dirs "${PROJECT_SOURCE_DIR}/third-party;${PROJECT_BINARY_DIR}/include;${PROJECT_SOURCE_DIR}/third-party/chibi-scheme/include")


#------------------------------------------------------------------------------
add_library(textbook-core
  casefold.cpp
  casefold.hpp
  utils.cpp
  utils.hpp
  nodeclass.cpp
  nodeclass.hpp
  nodelist.cpp
  nodelist.hpp
  nodes.cpp
  nodes.hpp
  nodeutils.cpp
  nodeutils.hpp
  textbook-model.hpp
  textbook-parser.cpp
  textbook-parser.hpp
  textbook-scanner.cpp
  textbook-scanner.hpp
  fo.hpp
  fos.cpp
  fos.hpp
  sosofo.cpp
  sosofo.hpp
  colors.hpp
  colors.cpp
  abstract-processor.hpp
  abstract-processor.ipp
  fo-processor.hpp
  processor.hpp
  propstack.cpp
  propstack.hpp
  html-processor.cpp
  html-processor.hpp
  html-writer.cpp
  html-writer.hpp
  tex-processor.cpp
  tex-processor.hpp
  debug-processor.cpp
  debug-processor.hpp
)
target_include_directories(textbook-core PUBLIC "${include_dirs}")
target_compile_options(textbook-core
  PUBLIC ${cxx11_options} ${warning_options})


#------------------------------------------------------------------------------
add_definitions(-DSEXP_USE_KEYWORDS=1 -DSEXP_USE_QUANTITY=1)

add_executable(textbook
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
target_include_directories(textbook PUBLIC "${include_dirs}")
target_compile_options(textbook
  PUBLIC ${cxx11_options} ${warning_options})
target_link_libraries(textbook
  textbook-core
  scheme
  program_options
  ${fspp_LIBRARIES}
  ${CMAKE_DL_LIBS})

set(textbook_app "${PROJECT_BINARY_DIR}/app")
set_target_properties(textbook
  PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${textbook_app}")
set_target_properties(textbook
  PROPERTIES RUNTIME_OUTPUT_DIRECTORY_DEBUG "${textbook_app}")
set_target_properties(textbook
  PROPERTIES RUNTIME_OUTPUT_DIRECTORY_RELEASE "${textbook_app}")


if(POLICY CMP0026)
cmake_policy(PUSH)
cmake_policy(SET CMP0026 OLD)
endif()

get_property(textbook_executable_path TARGET textbook PROPERTY LOCATION)
configure_file("${PROJECT_SOURCE_DIR}/bin/textbook.in" "${PROJECT_SOURCE_DIR}/bin/textbook"
               @ONLY)

if(POLICY CMP0026)
cmake_policy(POP)
endif()


install(TARGETS textbook
        RUNTIME DESTINATION bin)


#------------------------------------------------------------------------------
add_subdirectory(test)
