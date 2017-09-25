;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

(define (match-element? pattern snl)
  (let loop ((nlp snl)
             (p pattern))
    (cond
     ((null? p) #t)
     ((equal? (car p) (gi nlp)) (loop (parent nlp) (cdr p)) )

     (else #f))
    ))

(define (select-elements nl pattern)
  (let ((pattern-lst (if (list? pattern)
                         pattern
                         (list pattern))))
    (let loop ((n nl)
               (result (node-list)))
      (if (node-list-empty? n)
          result
          (loop (node-list-rest n) (let ((snl (node-list-first n)))
                                     (if (match-element? pattern-lst snl)
                                         (node-list result snl)
                                         result))) )
      )))
