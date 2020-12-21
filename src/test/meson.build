# -*-meson-*-
# Copyright (c) 2020 Gregor Klinke
# All rights reserved.


nodes_tests = executable('nodes_tests',
                       [ 'tst_main.cpp',
                         'tst_nodes.cpp',
                         'tst_nodelist.cpp',
                         'tst_nodeutils.cpp',
                       ],
                       include_directories : [catch_inc],
                       dependencies : [tbcore_dep, fspp_dep],
                       install : false)

test('nodes', nodes_tests,
     workdir : meson.source_root())
# Once we can require meson 0.56
#     workdir : meson.project_source_root())


utils_tests = executable('utils_tests',
                         [ 'tst_main.cpp',
                           'tst_utils.cpp',
                         ],
                         include_directories : [catch_inc],
                         dependencies : [tbcore_dep, fspp_dep],
                         install : false)
test('utils', utils_tests,
     workdir : meson.source_root())


tb_tests = executable('tb_tests',
                      [ 'tst_main.cpp',
                        'tst_textbook_parser.cpp',
                      ],
                      include_directories : [catch_inc],
                      dependencies : [tbcore_dep, fspp_dep],
                      install : false)
test('textbookscanner', tb_tests,
     workdir : meson.source_root())


parser_tests = executable('parser_tests',
                          [ 'parser_test.cpp', ],
                          include_directories : [catch_inc, json_inc],
                          dependencies : [tbcore_dep, fspp_dep],
                          install : false)
test('parser', parser_tests,
     workdir : meson.source_root(),
     args : ['-v',
             '--catalog', meson.source_root() / 'share' / 'textbook' / 'spec',
             meson.current_source_dir() / 'data' ])
