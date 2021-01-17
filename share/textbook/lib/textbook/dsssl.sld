;; -*-scheme-*-
;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

(define-library (textbook dsssl)
  (import (chibi)
          (srfi 1)  ;; list fold/reduce utilities
          (srfi 9)  ;; Defining Record Types
          (srfi 28) ;; basic string format
          (srfi 39) ;; parameter objects
          (srfi 69) ;; basic hash tables
          (srfi 89) ;; Optional positional and named parameters
          (srfi 95) ;; Sorting and Merging
          (textbook string-lib))
  (export %in-current-definition%
          attribute
          attributes
          current-mode
          current-node
          %with-current-node%
          default
          display-space
          element
          format-number
          id
          inline-space
          literal make-char
          make
          make-screen-set-model
          region
          match-element?
          mode
          named-node-list-names
          node-list
          node-list->list
          node-list-contains?
          node-list-difference
          node-list-every?
          node-list-filter
          node-list-head
          node-list-intersection
          node-list-map
          node-list-property
          node-list-reduce
          node-list-ref
          node-list-remove-duplicates
          node-list-reverse
          node-list-some?
          node-list-sublist
          node-list-tail
          node-list-union
          process-children
          process-children-trim
          process-children-trim
          process-node-list
          process-node-list-join
          process-node-list-trim
          register-default-rule
          register-element-rule
          register-root-rule
          root
          select-elements
          sosofo-append
          style style?
          text
          with-mode
          external-address
          current-node-address
          node-list-address
          address?
          address-local?
          )
  (include "nodelist.scm")
  (include "fo.scm")
  (include "dsssl.scm")
  (include "query.scm")
  (include "lang.scm")
  (include "process.scm")
  (include "default.scm")
  )
