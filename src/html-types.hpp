// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#pragma once

namespace eyestep {
namespace html {

  namespace detail {
    enum WrapStyle {
      k_normal_wrap,
      k_asis_wrap,
      k_no_wrap,
    };

    enum WsTreatment {
      k_preserve_ws,
      k_collapse_ws,
      k_ignore_ws,
    };


    struct StyleCtx {
      WsTreatment _wstreatment;
      WrapStyle _wrapstyle;
    };
  } // ns detail

} // ns html
} // ns eyestep
