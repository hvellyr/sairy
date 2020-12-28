# -*-meson-*-
# Copyright (c) 2020 Gregor Klinke
# All rights reserved.


conf_data.set('TEXTBOOK_HAVE_STD_CODECVT', supports_std_codecvt)
#conf_data.set('TEXTBOOK_HAVE_STD_OPTIONAL', supports_std_optional)
conf_data.set('TEXTBOOK_HAVE_STD_MAKE_UNIQUE', supports_std_make_unique)
conf_data.set('TEXTBOOK_HAVE_STD_CODECVT', supports_std_codecvt)
conf_data.set('TEXTBOOK_HAVE_STD_ENABLE_IF_T', supports_std_enable_if_t)
#conf_data.set('TEXTBOOK_HAVE_STD_STRINGSTREAM_COPYASSIGN', supports_stringstream_copyassign)
#conf_data.set('TEXTBOOK_HAVE_STD_MISMATCH_WITH_PREDICATE', supports_std_mismatch_with_pred)
conf_data.set('TEXTBOOK_HAVE_MISLEADING_INDENTATION_WARNING', supports_misleading_indentation_warning)

configure_file(input : 'config.hpp.in-meson',
               output : 'config.hpp',
               configuration : conf_data,
               install : false)

configuration_inc = include_directories('.')


tbcore_sources = [
  'casefold.cpp',
  'utils.cpp',
  'nodeclass.cpp',
  'nodelist.cpp',
  'nodes.cpp',
  'nodeutils.cpp',
  'textbook-parser.cpp',
  'textbook-scanner.cpp',
  'fos.cpp',
  'sosofo.cpp',
  'colors.cpp',
  'propstack.cpp',
  'html-processor.cpp',
  'html-writer.cpp',
  'tex-processor.cpp',
  'debug-processor.cpp',
]

tbcore_lib = static_library('textbook-core',
                            tbcore_sources,
                            include_directories : [configuration_inc, cxxopts_inc],
                            dependencies : fspp_dep,
                            install : false)

tbcore_dep = declare_dependency(include_directories : [configuration_inc, cxxopts_inc],
                                link_with : tbcore_lib,
                                dependencies : fspp_dep)

tblexicon_sources = [
  'lexicon-scanner.cpp',
]

tblexicon_lib = static_library('textbook-lexicon',
                               tblexicon_sources,
                               dependencies : tbcore_dep,
                               install : false)

textbook_sources = [
  'main.cpp',
  'processor-setup.cpp',
  'scanner-setup.cpp',
  'scm-context.cpp',
  'style-engine.cpp',
]

textbook = executable('textbook',
                      textbook_sources,
                      include_directories : chibi_inc,
                      link_with : [tblexicon_lib, chibi_lib],
                      cpp_args : ['-DSEXP_USE_KEYWORDS=1', '-DSEXP_USE_QUANTITY=1'],
                      dependencies : tbcore_dep,
                      install : true)

subdir('test')
