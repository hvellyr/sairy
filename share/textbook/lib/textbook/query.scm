;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

(define (vector-any? pred? v)
  (let ((vlen (vector-length v)))
    (let loop ((i 0))
      (if (>= i vlen)
          #f
          (if (pred? (vector-ref v i))
              #t
              (loop (+ i 1)))))))


(define (match-element-impl? pattern snl)
  (let loop ((nd snl)
             (p pattern))
    (if (null? p)
        #t
        (let ((step (car p)))
          (cond
           ((equal? step (gi nd))
            (loop (parent nd) (cdr p)) )

           ((and (vector? step) (> (vector-length step) 1))
            (case (vector-ref step 0)
              ((gi:) (if (equal? (vector-ref step 1) (gi nd))
                         (loop (parent nd) (cdr p))
                         #f))
              ((gis:) (let ((patterns (vector-ref step 1)))
                        (if (and (list? patterns) (member (gi nd) patterns))
                            (loop (parent nd) (cdr p))
                            #f)))
              (else #f)))
           (else #f)))
        )))

(define (match-element? pattern snl)
  (match-element-impl? (if (list? pattern)
                           (reverse pattern)
                           (list pattern)) snl))

(define (select-elements nl pattern)
  (let* ((pattern-lst (if (list? pattern)
                          (reverse pattern)
                          (list pattern)))
        (result (node-list-filter (lambda (snl)
                                    (match-element-impl? pattern-lst snl))
                                  nl)))
    result))
