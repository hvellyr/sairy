;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

(define-syntax quote-arg
  (syntax-rules ()
    ((quote-arg v) (if (symbol? v) 'v v))
    ))

(define-syntax quote-args
  (syntax-rules ()
    ((quote-args) '())
    ((quote-args e) (list (quote-arg e)))
    ((quote-args e1 e2 ...)
     (begin
       (cons (quote-arg e1) (quote-args e2 ...))) )
    ))


(define-syntax make
  (syntax-rules ()
    ((make fo-class) (%make-fo 'fo-class #()))
    ((make fo-class args ...) (%make-fo 'fo-class
                                        (list->vector (quote-args args ...))))
    ))

(define-syntax literal
  (syntax-rules ()
    ((literal str) (%make-fo 'literal (vector text: str)))
    ))
