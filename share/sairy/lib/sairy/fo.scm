;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

(define-syntax %args
  (syntax-rules ()
    ((%args) '())
    ((%args e) (list (if (symbol? e) 'e e)))
    ((%args e1 e2 ...)
     (begin
       (cons (if (symbol? e1) 'e1 e1) (%args e2 ...))) )))

(define-syntax make
  (syntax-rules ()
    ((make fo-class) (%make-fo 'fo-class #()))
    ((make fo-class args ...) (%make-fo 'fo-class (list->vector (%args args ...))))
    ))


(define-syntax literal
  (syntax-rules ()
    ((literal str) (%make-fo 'literal (vector 'text: str)))
    ))
