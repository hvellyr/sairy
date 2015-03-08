;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

(define-syntax make
  (syntax-rules ()
    ((make fo-class) (%make-fo 'fo-class #()))
    ((make fo-class args ...) (%make-fo 'fo-class #(args ...)))
    ))
