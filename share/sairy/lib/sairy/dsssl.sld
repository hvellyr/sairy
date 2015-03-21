(define-library (sairy dsssl)
  (import (chibi) (srfi 9) (srfi 28) (srfi 39) (srfi 69))
  (export current-mode register-default-rule register-root-rule register-element-rule
          default element root mode process-node-list process-children current-node
          make literal dimen)
          node-list
  (include "dsssl.scm")
  (include "fo.scm"))
