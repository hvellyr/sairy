;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

;; requires r7rs

;; support define-macro
(define-syntax define-macro
  (syntax-rules ()
    ((define-macro (name . args) body ...)
     (define-syntax name
       (rsc-macro-transformer
        (let ((transformer (lambda args body ...)))
          (lambda (exp env)
            (apply transformer (cdr exp))))
        )
       ))
    ))
