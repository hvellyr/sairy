# Copyright (c) 2015 Gregor Klinke
# All rights reserved.

add_executable(nodes_test
  tst_main.cpp
  tst_nodes.cpp
  tst_nodelist.cpp
  tst_nodeutils.cpp
)

target_link_libraries(nodes_test
  cppscanner
  grove)

add_test(nodes_test_case nodes_test)
