// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "scm-context.hpp"
#include "estd/memory.hpp"

#include "chibi/eval.h"

#include <iostream>
#include <string>
#include <memory>


namespace eyestep {

namespace fs = boost::filesystem;

namespace {
  bool check_exception_p(sexp ctx, sexp res)
  {
    sexp_gc_var3(err, sym, tmp);

    if (res && sexp_exceptionp(res)) {
      sexp_gc_preserve3(ctx, err, sym, tmp);
      tmp = res;
      err = sexp_current_error_port(ctx);
      if (!sexp_oportp(err)) {
        err = sexp_make_output_port(ctx, stderr, SEXP_FALSE);
      }

      sexp_print_exception(ctx, res, err);
      sexp_stack_trace(ctx, err);

      sexp_gc_release3(ctx);

      return false;
    }
    return true;
  }


  class SchemeContext : public ISchemeContext {
    sexp mCtx;

  public:
    SchemeContext() : mCtx(nullptr) {}


    ~SchemeContext()
    {
      if (mCtx != nullptr) {
        sexp_destroy_context(mCtx);
      }
    }


    void initialize(const fs::path& modulePath) override
    {
      mCtx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);

      sexp_gc_var1(tmp);
      sexp_gc_preserve1(mCtx, tmp);

      std::string libPath = modulePath.string();

      sexp_add_module_directory(mCtx,
                                tmp = sexp_c_string(mCtx, libPath.c_str(), -1),
                                SEXP_FALSE);
      libPath = libPath + "/lib";
      sexp_add_module_directory(mCtx,
                                tmp = sexp_c_string(mCtx, libPath.c_str(), -1),
                                SEXP_FALSE);

      sexp_load_standard_env(mCtx, NULL, SEXP_SEVEN);
      sexp_load_standard_ports(mCtx, NULL, stdin, stdout, stderr, 1);

      sexp_gc_release1(mCtx);
    }


    bool loadScript(const fs::path& scriptFile) override
    {
      sexp_gc_var2(obj1, obj2);
      sexp_gc_preserve2(mCtx, obj1, obj2);

      obj1 = sexp_c_string(mCtx, scriptFile.string().c_str(), -1);
      bool retv = check_exception_p(mCtx, sexp_load(mCtx, obj1, NULL));

      sexp_gc_release2(mCtx);

      return retv;
    }
  };
} // ns anon


std::unique_ptr<ISchemeContext> createSchemeContext()
{
  return estd::make_unique<SchemeContext>();
}

} // ns eyestep
