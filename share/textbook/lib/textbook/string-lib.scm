;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

;; we should really have an srfi-13 implementation

(define (whitespace? c)
  (or (char=? c #\space)
      (char=? c #\newline)
      (char=? c #\tab)
      (char=? c #\return) ))

(define (string-empty? str)
  (= (string-length str) 0))


;; @doc Returns the index of the first occurance of a character in @prm{str} for
;; while @prm{pred} returns #f or #f if no character matches.
(define (string-find str pred)
  (let ((slen (string-length str)))
    (let loop ((i 0))
      (if (>= i slen)
          #f
          (if (not (pred (string-ref str i)))
              i
              (loop (+ i 1)))))))

(define (string-find-right str pred)
  (let loop ((i (- (string-length str) 1)))
      (if (< i 0)
          #f
          (if (not (pred (string-ref str i)))
              (loop (- i 1))
              i))))

;; @doc Indicates whether a string @prm{str} contains at least one occurance of
;; @prm{c}.
(define (string-contains? str c)
  (integer? (string-find str (lambda (c2) (char=? c c2)))))

;; @doc Returns a new string without all characters from the start which match
;; the @proc{whitespace?} predicate.
(define (string-trim str)
  (let ((idx (string-find str whitespace?)))
    (if idx
        (substring str idx (string-length str))
        "")))

;; @doc Returns a new string without all characters from the end which match the
;; @proc{whitespace?} predicate.
(define (string-trim-right str)
  (let ((idx (string-find-right str whitespace?)))
    (if idx
        (substring str 0 (+ idx 1))
        "")))

;; @doc Returns a new string with all characters matching the @proc{whitespace?}
;; predicate removed from the start and end of @prm{str}.
(define (string-trim-both str)
  (let ((idx-left (string-find str whitespace?))
        (idx-right (string-find-right str whitespace?)))
    (if (and idx-left idx-right)
        (substring str idx-left (+ idx-right 1))
        "")))

;; @doc Creates a new string with length @prm{k} by copying the right most
;; @prm{k} characters from @prm{s} into its right end.  The remaining left – if
;; any – characters are filled with character @prm{c}.  If @prm{s} doesn't fit
;; into @prm{k} characters its truncated.
;;
;; @example
;; (string-pad-left "1234"    5 #\space)   @result " 1234".
;; (string-pad-left "1234567" 5 #\space)   @result "34567".
;; @end example
(define (string-pad-left s k c)
  (let ((pad-k (- k (string-length s))))
    (cond
     ((< pad-k 0) (string-copy s (abs pad-k) (string-length s)))
     ((= pad-k 0) s)
     (else (string-append (make-string pad-k c) s)) )))

;; @doc Creates a new from the upper case counterpart to string @prm{s}.
(define (string-upcase-ascii s)
  (list->string
   (map (lambda (c) (char-upcase c))
        (string->list s))))

;; @doc Creates a new from the lower case counterpart to string @prm{s}.
(define (string-downcase-ascii s)
  (list->string
   (map (lambda (c) (char-downcase c))
        (string->list s))))

;; @doc Indicates whether a string @prm{str0} is a prefix of string @prm{str1}.
;; If both strings are identical (according to @fun{eqv?}) the result is
;; @c{true}.
;;
;; @example
;; (string-prefix? "abc" "def")  @result #f
;; (string-prefix? "a" "abc")    @result #t
;; (string-prefix? "abc" "abc")  @result #t
;; (string-prefix? "" "")        @result #t
;; (string-prefix? "b" "abc")    @result #f
;; @end example
;; (define (string-prefix? str0 str1)
;;   (let ((len0 (string-length str0))
;;         (len1 (string-length str1)))
;;     (cond
;;      ((> len0 len1) #f)
;;      ((= len0 len1) (string=? str0 str1))
;;      (else (let ((len (min len0 len1)))
;;              (let loop ((i0 0)
;;                         (i1 0))
;;                (if (>= i0 len)
;;                    #t
;;                    (if (char=? (string-ref str0 i0) (string-ref str1 i1))
;;                        (loop (+ i0 1) (+ i1))
;;                        #f))))))))


;; @doc Joins all elements from @prm{strlist} which must be strings into a
;; resulting string separated by @prm{sep}.  If @prm{strlist} is empty the
;; resulting string is empty.
(define (string-join strlist sep)
  (if (= (length strlist) 0)
      ""
      (let loop ((p (cdr strlist))
                 (result (car strlist)))
        (if (null? p)
            result
            (loop (cdr p) (string-append result sep (car p)))))))


;; @doc Split a string @prm{str} at @prm{separator} and returns a list of the
;; resulting strings.  Chains of @prm{sep} lead to empty strings in the
;; resulting list.
(define (string-split str sep)
  (let ((slen (string-length str)))
    (let loop ((fp 0)
               (lp 0)
               (lines '()))
      (if (>= lp slen)
          (append lines (list (string-copy str fp lp)))
          (if (char=? (string-ref str lp) sep)
              (loop (+ lp 1)  ;; fp = lp + 1
                    (+ lp 1)  ;; lp = lp + 1
                    (append lines (list (string-copy str fp lp))))
              (loop fp (+ lp 1) lines) )
          ))))
