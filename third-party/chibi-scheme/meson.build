# -*-meson-*-
# Copyright (c) 2020 Gregor Klinke
# All rights reserved.


chibi_inc = include_directories('dist', 'dist/include')

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
chibi_private_flags = []

if supports_no_shift_negative_value_warning
  chibi_flags += '-Wno-shift-negative-value'
endif
if supports_no_expansion_to_defined_warning
  chibi_flags += '-Wno-expansion-to-defined'
endif

ccomp = meson.get_compiler('c')

if host_machine.system() == 'linux'
  if ccomp.has_argument('-Wno-sign-compare')
    chibi_private_flags += '-Wno-sign-compare'
  endif
  if ccomp.has_argument('-Wno-unused-but-set-variable')
    chibi_private_flags += '-Wno-unused-but-set-variable'
  endif
  if ccomp.has_argument('-Wno-stringop-truncation')
    chibi_private_flags += '-Wno-stringop-truncation'
  endif
  if ccomp.has_argument('-Wno-unused-but-set-variable')
    chibi_private_flags += '-Wno-unused-but-set-variable'
  endif
endif

chibi_sources = [
  'src/clibs.c',
  'dist/gc.c',
  'dist/sexp.c',
  'dist/bignum.c',
  'dist/opcodes.c',
  'dist/vm.c',
  'dist/eval.c',
  'dist/simplify.c',
]

dl_dep = cc.find_library('dl', required : false)

chibi_lib = static_library('scheme',
                           chibi_sources,
                           include_directories : [chibi_inc],
                           c_args : chibi_args + chibi_flags + chibi_private_flags,
                           install : false)

chibi_dep = declare_dependency(include_directories : [chibi_inc],
                               link_with : chibi_lib,
                               compile_args : chibi_args + chibi_flags,
                               dependencies : dl_dep)

install_data([ 'dist/lib/init-7.scm',
               'dist/lib/meta-7.scm', ],
             install_dir : get_option('datadir') / 'textbook' / 'lib')
install_data([ 'dist/lib/srfi/1.sld',
               'dist/lib/srfi/6.sld',
               'dist/lib/srfi/9.sld', 'dist/lib/srfi/9.scm',
               'dist/lib/srfi/39.sld',
               'dist/lib/srfi/69.sld',
               'dist/lib/srfi/95.sld',
             ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi')

install_data([ 'dist/lib/srfi/1/alists.scm',
               'dist/lib/srfi/1/constructors.scm',
               'dist/lib/srfi/1/deletion.scm',
               'dist/lib/srfi/1/fold.scm',
               'dist/lib/srfi/1/lset.scm',
               'dist/lib/srfi/1/misc.scm',
               'dist/lib/srfi/1/predicates.scm',
               'dist/lib/srfi/1/search.scm',
               'dist/lib/srfi/1/selectors.scm',
             ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi' / '1')

install_data([ 'dist/lib/srfi/39/syntax-no-threads.scm',
               'dist/lib/srfi/39/syntax.scm',
             ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi' / '39')

install_data([ 'dist/lib/srfi/69/interface.scm',
               'dist/lib/srfi/69/type.scm',
             ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi' / '69')

install_data([ 'dist/lib/srfi/95/sort.scm', ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'srfi' / '95')

install_data([ 'dist/lib/scheme/cxr.scm',
               'dist/lib/scheme/cxr.sld', ],
             install_dir : get_option('datadir') / 'textbook' / 'lib' / 'scheme')
