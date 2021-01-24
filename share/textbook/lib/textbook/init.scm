;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

(import (srfi 1))
(import (srfi 28))
(import (srfi 89))
(import (srfi 95))
(import (textbook dsssl))
(import (textbook string-lib))
(import (textbook type-of))


(define builtin-add +)
(define builtin-sub -)
(define builtin-mul *)
(define builtin-div /)

(define (+ . args)
  (cond
   ((length-spec? (car args)) (length-spec-add args))
   (else (apply builtin-add args))))

(define (- . args)
  (cond
   ((length-spec? (car args)) (length-spec-sub args))
   (else (apply builtin-sub args))))

(define (* . args)
  (cond
   ((length-spec? (car args)) (length-spec-multiply args))
   (else (apply builtin-mul args))))

(define (/ . args)
  (cond
   ((length-spec? (car args)) (length-spec-divide args))
   (else (apply builtin-div args))))
