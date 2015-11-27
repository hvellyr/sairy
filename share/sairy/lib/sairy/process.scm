;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

(import (srfi 6))

;; @doc Returns the sosofo that results from appending the sosofos that result
;; from processing in order the children of the current node.
(define (process-children)
  (process-node-list (children (current-node))))

;; @doc Returns the sosofo that results from appending the sosofos that result
;; from processing in order the children of the current node after removing any
;; leading and trailing whitespace from leading and trailing text nodes.
(define (process-children-trim)
  (process-node-list-trim (children (current-node))))
