(define-library (sairy dsssl)
  (import (chibi) (srfi 9) (srfi 28) (srfi 39) (srfi 69) (srfi 89)
          (sairy string-lib))
  (export current-mode register-default-rule register-root-rule register-element-rule
          default element root mode text
          process-node-list process-node-list-trim
          process-children process-children-trim current-node
          make literal inline-space display-space
          select-elements match-element?
          node-list
          sosofo-append
          node-list-property node-list-reduce node-list-map node-list->list
          node-list-reverse node-list-ref node-list-tail node-list-sublist
          named-node-list-names
          attributes attribute id
          )
  (include "nodelist.scm")
  (include "fo.scm")
  (include "dsssl.scm")
  (include "query.scm")
  )
