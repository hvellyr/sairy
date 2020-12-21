// Copyright (c) 2020 Gregor Klinke
// All rights reserved.

#include "chibi/eval.h"

#define sexp_init_library sexp_init_lib_srfi_39
#include "dist/lib/srfi/39/param.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_srfi_69
#include "dist/lib/srfi/69/hash.c"
#undef sexp_init_library

#define sexp_init_library sexp_init_lib_srfi_95
#include "dist/lib/srfi/95/qsort.c"
#undef sexp_init_library


struct sexp_library_entry_t sexp_static_libraries_array[] = {
  { "lib/srfi/39/param", sexp_init_lib_srfi_39 },
  { "lib/srfi/69/hash", sexp_init_lib_srfi_69 },
  { "lib/srfi/95/qsort", sexp_init_lib_srfi_95 },
  { NULL, NULL }
};

struct sexp_library_entry_t* sexp_static_libraries = sexp_static_libraries_array;
