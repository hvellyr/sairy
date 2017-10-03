;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

(define-library (textbook dsssl)
  (import (chibi) (srfi 9) (srfi 28) (srfi 39) (srfi 69) (srfi 89) (srfi 95)
          (textbook string-lib))
  (export %in-current-definition%
          attribute
          attributes
          current-mode
          current-node
          default
          display-space
          element
          format-number
          id
          inline-space
          literal
          make
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
          style
          text
          with-mode
          )
  (include "nodelist.scm")
  (include "fo.scm")
  (include "dsssl.scm")
  (include "query.scm")
  (include "lang.scm")
  (include "process.scm")
  )
