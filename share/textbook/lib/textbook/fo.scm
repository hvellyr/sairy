;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

(import (srfi 28))


(define (make-source def)
  (cons (format "definition of '~s'" def) #f))

(define-syntax make
  (syntax-rules ()
    ((make fo-class) (%make-fo 'fo-class '() (make-source (%in-current-definition%))))
    ((make fo-class args ...) (%make-fo 'fo-class (list args ...) (make-source (%in-current-definition%))))
    ))


(define-syntax literal
  (syntax-rules ()
    ((literal str) (%make-fo 'literal (list text: str) (make-source (%in-current-definition%))))
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


(define-syntax region
  (syntax-rules ()
    ((region args ...) (list args ...))))

(define-syntax make-screen-set-model
  (syntax-rules ()
    ((make-screen-set-model regions ...) (%make-screen-set-model
                                          (list regions ...)
                                          (make-source (%in-current-definition%))))))
