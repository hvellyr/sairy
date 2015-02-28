(define-library (srfi 88)
  (export keyword? keyword->string string->keyword)
  (import (chibi))
  (include "88/keywords.scm"))
