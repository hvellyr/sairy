(define-library (sairy string-lib)
  (import (chibi))
  (export string-trim string-trim-right string-trim-both
          string-pad-left
          string-upcase-ascii string-downcase-ascii
          string-prefix?
          string-join string-split string-find
          string-contains?
          )
  (include "string-lib.scm")
  )
