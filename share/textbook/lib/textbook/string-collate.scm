;; Copyright (c) 2019 by Gregor Klinke
;; All rights reserved.

(import (srfi 28))
(import (srfi 9)) ;; define record types
(import (srfi 69)) ;; hash
(import (textbook trie))
(import (textbook string-lib))


(define-record-type :collation
    (collation _dict _to-lower _to-upper _other-weight)
    collation?
    (_dict collation-dict)
    (_to-lower collation-to-lower-case)
    (_to-upper collation-to-upper-case)
    (_other-weight collation-other-weight)
    )


(define-record-type :collation-string
    (collation-string _data)
    collation-string?
    (_data collation-string-data))


(define (make-collation sort-str)
  (define (add-enc-str encstr dict to-lower to-upper start-weight)
    (let loop ((i-str (string-split encstr #\;))
               (pos start-weight))
      (if (null? i-str)
          (+ pos 1)
          (begin
            (for-each (lambda (str)
                        (let* ((str0 (string-trim str))
                               (case-conv (string-split str0 #\=)))
                          (case (length case-conv)
                            ((2) (begin
                                   (trie-insert dict (list-ref case-conv 0) pos)
                                   (trie-insert dict (list-ref case-conv 1) pos)
                                   (hash-table-set! to-lower (list-ref case-conv 0) (list-ref case-conv 1))
                                   (hash-table-set! to-upper (list-ref case-conv 1) (list-ref case-conv 0))
                                   ))
                            ((1) (begin
                                   (trie-insert dict str0 pos)
                                   (hash-table-set! to-lower str0 str0)
                                   (hash-table-set! to-upper str0 str0)
                                   ))
                            (else
                             (display (format "Syntax problem of collation format: ~s~%" str0)))
                            )
                          ))
                      (string-split (string-trim (car i-str)) #\,))
            (loop (cdr i-str) (+ pos 1))))))
  (let* ((dict (make-trie))
         (to-lower (make-hash-table))
         (to-upper (make-hash-table))
         (next-weight (add-enc-str sort-str dict to-lower to-upper 1))
         )
    (collation dict to-lower to-upper next-weight)
    ))


(define (test-longest-substr str coll-dict slen beg len prev-weight prev-len)
  (let* ((cur-str (substring str beg (+ beg len)))
         (weight (trie-ref coll-dict cur-str))
         )
    (if (or (eqv? weight 'not-found)
            (trie-empty-value? weight))
        (vector prev-weight prev-len (substring str beg (+ beg prev-len)))
        (if (or (not (trie-prefix? coll-dict cur-str))
                (>= (+ beg len) slen))
            (vector weight len cur-str)
            (test-longest-substr str coll-dict slen beg (+ len 1)
                                 weight len)))))


(define (collate-string->weights coll str)
  (let* ((slen (string-length str))
         (coll-dict (collation-dict coll))
         (other-weight (collation-other-weight coll))
         )
    (let loop ((ofs 0)
               (result (list)))
      (if (>= ofs slen)
          (collation-string result)
          (let* ((subcol (test-longest-substr str coll-dict slen ofs 1 other-weight 1)))
            (loop (+ ofs (vector-ref subcol 1))
                  (append result (list (vector-ref subcol 0)))))
          ))))


(define (collate-string->list coll str)
  (let* ((slen (string-length str))
         (coll-dict (collation-dict coll))
         (other-weight (collation-other-weight coll))
         )
    (let loop ((ofs 0)
               (result (list)))
      (if (>= ofs slen)
          result
          (let* ((subcol (test-longest-substr str coll-dict slen ofs 1 other-weight 1))
                 (col-substr (hash-table-ref/default
                              (collation-to-lower-case coll)
                              (vector-ref subcol 2)
                              (vector-ref subcol 2))))
            (loop (+ ofs (vector-ref subcol 1))
                  (append result (list col-substr))))
          ))))


(define (collation-string=? c0 c1)
  (if (not (= (length (collation-string-data c0)) (length (collation-string-data c1))))
      #f
      (let loop ((p0 (collation-string-data c0))
                 (p1 (collation-string-data c1)))
        (if (null? p0)
            #t
            (if (not (= (car p0) (car p1)))
                #f
                (loop (cdr p0) (cdr p1)))))))


(define (collation-string<? cs0 cs1)
  (let loop ((p0 (collation-string-data cs0))
             (p1 (collation-string-data cs1))
             (c0 #\space)
             (c1 #\space)
             )
    (cond
     ((null? p0) (if (null? p1)
                     (< c0 c1)
                     (<= c0 c1)))
     ((null? p1) #f)
     ((< (car p0) (car p1)) #t)
     ((> (car p0) (car p1)) #f)
     (else (loop (cdr p0) (cdr p1) (car p0) (car p1))
           ))
    ))


(define (string-collated=? coll s0 s1)
  (collation-string=? (collate-string->weights coll s0) (collate-string->weights coll s1)))

(define (string-collated<? coll s0 s1)
  (collation-string<? (collate-string->weights coll s0) (collate-string->weights coll s1)))
