# -*-meson-*-
# Copyright (c) 2020 Gregor Klinke
# All rights reserved.


cxxopts_inc = include_directories('cxxopts/include')


# ------------------------------------------------------------------------------

chibi_inc = include_directories('chibi-scheme', 'chibi-scheme/include')

chibi_args = [
  '-DSEXP_USE_MODULES=1',
  '-DSEXP_USE_BOEHM=0',
  '-DSEXP_USE_UTF8_STRINGS=1',
  '-DSEXP_USE_KEYWORDS=1',
  '-DSEXP_USE_QUANTITY=1',
  '-DSEXP_INITIAL_HEAP_SIZE=42*1024*1024',
  '-DSEXP_USE_STATIC_LIBS=1',
  '-DSEXP_USE_STATIC_LIBS_NO_INCLUDE=1',
]

chibi_flags = []
if supports_no_shift_negative_value_warning
  chibi_flags += '-Wno-shift-negative-value'
endif
if supports_no_expansion_to_defined_warning
  chibi_flags += '-Wno-expansion-to-defined'
endif

chibi_sources = [
  'build/chibi-scheme/clibs.c',
  'chibi-scheme/gc.c',
  'chibi-scheme/sexp.c',
  'chibi-scheme/bignum.c',
  'chibi-scheme/opcodes.c',
  'chibi-scheme/vm.c',
  'chibi-scheme/eval.c',
  'chibi-scheme/simplify.c',
]

chibi_lib = static_library('scheme',
                           chibi_sources,
                           include_directories : [chibi_inc],
                           c_args : chibi_args + chibi_flags,
                           install : false)

install_data([ 'chibi-scheme/lib/init-7.scm',
               'chibi-scheme/lib/meta-7.scm', ],
             install_dir : get_option('datadir') / 'textbook' / 'lib')
install_data([ 'chibi-scheme/lib/srfi/1.sld',
               'chibi-scheme/lib/srfi/6.sld',
               'chibi-scheme/lib/srfi/9.sld', 'chibi-scheme/lib/srfi/9.scm',
               'chibi-scheme/lib/srfi/39.sld',
               'chibi-scheme/lib/srfi/69.sld',
               'chibi-scheme/lib/srfi/95.sld',
             ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi')

install_data([ 'chibi-scheme/lib/srfi/1/alists.scm',
               'chibi-scheme/lib/srfi/1/constructors.scm',
               'chibi-scheme/lib/srfi/1/deletion.scm',
               'chibi-scheme/lib/srfi/1/fold.scm',
               'chibi-scheme/lib/srfi/1/lset.scm',
               'chibi-scheme/lib/srfi/1/misc.scm',
               'chibi-scheme/lib/srfi/1/predicates.scm',
               'chibi-scheme/lib/srfi/1/search.scm',
               'chibi-scheme/lib/srfi/1/selectors.scm',
             ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi' / '1')

install_data([ 'chibi-scheme/lib/srfi/39/syntax-no-threads.scm',
               'chibi-scheme/lib/srfi/39/syntax.scm',
             ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi' / '39')

install_data([ 'chibi-scheme/lib/srfi/69/interface.scm',
               'chibi-scheme/lib/srfi/69/type.scm',
             ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi' / '69')

install_data([ 'chibi-scheme/lib/srfi/95/sort.scm', ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi' / '95')

install_data([ 'chibi-scheme/lib/scheme/cxr.scm',
               'chibi-scheme/lib/scheme/cxr.sld', ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'scheme')
