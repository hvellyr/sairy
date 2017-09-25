;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

(define %roman-numerals%
  #("0" "i" "ii" "iii" "iv" "v" "vi" "vii" "viii" "ix"
    "x" "xi" "xii" "xiii" "xiv" "xv" "xvi" "xvii" "xviii" "xix"
    "xx" "xxi" "xxii" "xxiii" "xxiv" "xxv" "xxvi" "xxvii" "xxviii" "xxix"
    "xxx" "xxxi" "xxxii" "xxxiii" "xxxiv" "xxxv" "xxxvi" "xxxvii" "xxxviii" "xxxix"
    "xl" "xli" "xlii" "xliii" "xliv" "xlv" "xlvi" "xlvii" "xlviii" "xlix"
    "l" "li" "lii" "liii" "liv" "lv" "lvi" "lvii" "lviii" "lix"
    "lx" "lxi" "lxii" "lxiii" "lxiv" "lxv" "lxvi" "lxvii" "lxviii" "lxix"
    "lxx" "lxxi" "lxxii" "lxxiii" "lxxiv" "lxxv" "lxxvi" "lxxvii" "lxxviii" "lxxix"
    "lxxx" "lxxxi" "lxxxii" "lxxxiii" "lxxxiv" "lxxxv" "lxxxvi" "lxxxvii" "lxxxviii" "lxxxix"
    "xc" "xci" "xcii" "xciii" "xciv" "xcv" "xcvi" "xcvii" "xcviii" "xcix"
    "c" "ci" "cii" "ciii" "civ" "cv" "cvi" "cvii" "cviii" "cix"
    "cx" "cxi" "cxii" "cxiii" "cxiv" "cxv" "cxvi" "cxvii" "cxviii" "cxix"
    "cxx" "cxxi" "cxxii" "cxxiii" "cxxiv" "cxxv" "cxxvi" "cxxvii" "cxxviii" "cxxix"))

(define %alpha-numerals%
  #("0" "a" "b" "c" "d" "e" "f" "g" "h" "i"
    "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"           ;; 10
    "t" "u" "v" "w" "x" "y" "z" "aa" "ab" "ac"        ;; 20
    "ad" "ae" "af" "ag" "ah" "ai" "aj" "ak" "al" "am" ;; 30
    "an" "ao" "ap" "aq" "ar" "as" "at" "au" "av" "ax" ;; 40
    "ay" "az" "ba" "bc" "bd" "be" "bf" "bg" "bh" "bi" ;; 50
    "bj" "bk" "bl" "bm" "bn" "bo" "bp" "bq" "br" "bs" ;; 60
    "bt" "bu" "bv" "bw" "bx" "by" "bz" "ca" "cb" "cc" ;; 70
    "cd" "ce" "cf" "cg" "ch" "ci" "cj" "ck" "cl" "cm" ;; 80
    "cn" "co" "cp" "cq" "cr" "cs" "ct" "cu" "cv" "cw" ;; 90
    "cx" "cy" "cz" "da" "db" "dc" "dd" "de" "df" "dg" ;; 100
    "dh" "di" "dj" "dk" "dl" "dm" "dn" "do" "dp" "dq" ;; 110
    "dr" "ds" "dt" "du" "dv" "dw" "dx" "dy" "dz"      ;; 120
    ))

;; @doc Returns a string representation of @prm{n}. @p{format} specifies the
;; format to use as follows:
;;
;; @list
;; @item "1" means use 0, 1, 2 ...
;; @item "01" means use 00, 01, 02, ... 10, 11 ... 100, 101 ... and similarly
;;       for any number of leading zeros;
;; @item "a" means use 0, a, b, c, ... z, aa, ab, ...
;; @item "A" means use 0, A, B, C, ... Z, AA, AB, ...
;; @item "i" means use 0, i, ii, iii, iv, v, vi, vii, viii, ix, x, ...
;; @item "I" means use 0, I, II, III, IV, V, VI, VII, VIII, IX, X, ...
;; @end list
(define (format-number n format)
  (cond
   ((equal? format "1") (number->string n))
   ((string-prefix? "0" format) (let ((ns (number->string n)))
                                  (string-pad-left ns (string-length format) #\0)))
   ((equal? format "a") (vector-ref %alpha-numerals% n))
   ((equal? format "A") (string-upcase-ascii (vector-ref %alpha-numerals% n)))
   ((equal? format "i") (begin
                          (display "FORMAT i") (newline)
                          (vector-ref %roman-numerals% n)))
   ((equal? format "I") (string-upcase-ascii (vector-ref %roman-numerals% n)))
   (else (number->string n))
   ))
