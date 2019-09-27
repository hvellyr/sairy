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
add_library(lexicon
  lexicon-scanner.cpp
  lexicon-scanner.hpp
)
target_include_directories(lexicon PUBLIC "${include_dirs}")
target_compile_options(lexicon
  PUBLIC ${cxx11_options} ${warning_options})
target_link_libraries(lexicon
  textbook-core)


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
  lexicon
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


file(READ "${PROJECT_SOURCE_DIR}/bin/textbook.in" textbook_shell_exec_wrapper)
string(REPLACE "@textbook_executable_path@"
       "$<TARGET_FILE:textbook>" textbook_shell_exec_wrapper
       ${textbook_shell_exec_wrapper})
string(REPLACE "@project_source_dir@"
       ${PROJECT_SOURCE_DIR} textbook_shell_exec_wrapper
       ${textbook_shell_exec_wrapper})
file(GENERATE OUTPUT ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/textbook
  CONTENT ${textbook_shell_exec_wrapper})

file(WRITE "${CMAKE_BINARY_DIR}/copy_textbook_sh.cmake"
           "file(COPY ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/textbook
                 DESTINATION ${PROJECT_SOURCE_DIR}/bin
                 FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_WRITE GROUP_READ WORLD_READ)")
add_custom_command(TARGET textbook POST_BUILD
  COMMAND ${CMAKE_COMMAND} -P "${CMAKE_BINARY_DIR}/copy_textbook_sh.cmake")


install(TARGETS textbook
        RUNTIME DESTINATION bin)


#------------------------------------------------------------------------------
add_subdirectory(test)
