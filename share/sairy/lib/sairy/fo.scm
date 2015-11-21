;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

(define-syntax make
  (syntax-rules ()
    ((make fo-class) (%make-fo 'fo-class '()))
    ((make fo-class args ...) (%make-fo 'fo-class (list args ...)))
    ))


(define-syntax literal
  (syntax-rules ()
    ((literal str) (%make-fo 'literal (list text: str)))
    ))


(define-syntax sosofo-append
  (syntax-rules ()
    ((sosofo-append) (empty-sosofo))
    ((sosofo-append args ...) (%sosofo-append (list args ...)))
    ))


(define-syntax style
  (syntax-rules ()
    ((style args ...) (make-style (list args ...)))
    ))


(define* (inline-space q (min: min #f) (max: max #f))
  (%make-length-spec 'inline q (if (not min) q min)
                     (if (not max) q max)
                     #f
                     1))

(define* (display-space q
                        (min: min #f) (max: max #f)
                        (conditional?: cond #f)
                        (priority: prio 1))
  (%make-length-spec 'display q (if (not min) q min)
                     (if (not max) q max)
                     cond prio))
