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
