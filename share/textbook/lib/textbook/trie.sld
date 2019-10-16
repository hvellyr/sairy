;; -*-scheme-*-
;; Copyright (c) 2019 by Gregor Klinke
;; All rights reserved.

(define-library (textbook trie)
  (import (chibi))
  (export make-trie
          trie?
          trie-empty?
          trie-insert
          trie-ref trie-contains
          trie-fold trie-keys trie-values

          trie-empty-value?
          )
  (include "trie.scm")
  )
