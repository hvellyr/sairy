;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

;; we should really have an srfi-13 implementation

(define (whitespace? c)
  (or (char=? c #\space)
      (char=? c #\newline)
      (char=? c #\tab)
      (char=? c #\return) ))

;; @doc Returns a new string without all characters from the start which match
;; the @proc{whitespace?} predicate.
(define (string-trim str)
  (let loop ((i 0))
    (if (>= i (string-length str))
        str
        (let ((c (string-ref str i)))
          (if (whitespace? c)
              (loop (+ i 1))
              (substring str i (string-length str)))))
    ))

;; @doc Returns a new string without all characters from the end which match the
;; @proc{whitespace?} predicate.
(define (string-trim-right str)
  (let loop ((i (- (string-length str) 1)))
    (if (< i 0)
        str
        (let ((c (string-ref str i)))
          (if (whitespace? c)
              (loop (- i 1))
              (substring str 0 (+ i 1)))))
    ))

;; @doc Returns a new string with all characters matching the @proc{whitespace?}
;; predicate removed from the start and end of @prm{str}.
(define (string-trim-both str)
  (string-trim (string-trim-right str)))
