# Copyright (c) 2015 Gregor Klinke
# All rights reserved.

add_executable(nodes_test
  tst_main.cpp
  tst_nodes.cpp
  tst_nodelist.cpp
  tst_nodeutils.cpp
)
target_include_directories(nodes_test
  PUBLIC "${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}"
)

target_link_libraries(nodes_test
  sairy-utils
  grove
)

add_test(NAME nodes_test_case
         WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
         COMMAND nodes_test)


# ------------------------------------------------------------------------------

add_executable(utils_test
  tst_main.cpp
  tst_utils.cpp
)
target_include_directories(utils_test
  PUBLIC "${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}"
)
target_link_libraries(utils_test
  sairy-utils
  ${Boost_LIBRARIES}
)

add_test(NAME utils_test_case
         WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
         COMMAND utils_test)


# ------------------------------------------------------------------------------

add_executable(textbook_test
  tst_main.cpp
  tst_textbook_parser.cpp
)
target_include_directories(textbook_test
  PUBLIC "${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}"
)

target_link_libraries(textbook_test
  grove
  textbook-parser
  sairy-utils
  ${Boost_LIBRARIES}
)

add_test(NAME textbook_test_case
         WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
         COMMAND textbook_test)


# ------------------------------------------------------------------------------

add_executable(parser_test
  parser_test.cpp
)
target_include_directories(parser_test
  PUBLIC "${PROJECT_SOURCE_DIR}/third-party;${Boost_INCLUDE_DIRS}"
)

target_link_libraries(parser_test
  grove
  textbook-parser
  sairy-utils
  ${Boost_LIBRARIES}
)

add_test(NAME parser_test_case
         WORKING_DIRECTORY "${PROJECT_SOURCE_DIR}"
         COMMAND parser_test -v "${PROJECT_SOURCE_DIR}/src/test/data/")
