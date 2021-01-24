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

          inherited-above?
          inherited-asis-wrap-indent
          inherited-background-color
          inherited-background-tile
          inherited-below?
          inherited-bottom-margin
          inherited-box-corner-radius
          inherited-box-corner-rounded?
          inherited-class
          inherited-color
          inherited-end-indent
          inherited-end-margin
          inherited-field-align
          inherited-field-width
          inherited-first-line-start-indent
          inherited-font-caps
          inherited-font-name
          inherited-font-posture
          inherited-font-size
          inherited-font-weight
          inherited-footer-margin
          inherited-gutter-width
          inherited-header-margin
          inherited-inhibit-line-breaks?
          inherited-language
          inherited-last-line-end-indent
          inherited-left-margin
          inherited-line-number-side
          inherited-line-spacing
          inherited-line-thickness
          inherited-lines
          inherited-numbered-lines?
          inherited-page-height
          inherited-page-width
          inherited-position-point-shift
          inherited-quadding
          inherited-right-margin
          inherited-start-indent
          inherited-start-margin
          inherited-text
          inherited-top-margin
          inherited-whitespace-treatment
          )
  (include "nodelist.scm")
  (include "fo.scm")
  (include "characteristics.scm")
  (include "dsssl.scm")
  (include "query.scm")
  (include "lang.scm")
  (include "process.scm")
  (include "default.scm")
  )
