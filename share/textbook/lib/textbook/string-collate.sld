;; -*-scheme-*-
;; Copyright (c) 2019 by Gregor Klinke
;; All rights reserved.

(define-library (textbook string-collate)
  (import (chibi))
  (export make-collation collation?
          collate-string->weights
          collation-string?
          collation-string=? collation-string<?
          string-collated=? string-collated<?
          collate-string->list
          )
  (include "string-collate.scm")
  )
