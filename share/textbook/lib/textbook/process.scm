;; Copyright (c) 2017 by Gregor Klinke
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

;; @doc Returns the sosofo that results from appending the sosofos that result
;; from processing in order the elements of @prm{nl}.  Each sosofo such produced
;; is separated by @prm{sep} which should be a valid sosofo.
;;
;; If @prm{nl} contains zero or extactly one node @prm{sep} is not added at all.
(define (process-node-list-join nl sep)
  (if (= (node-list-length nl) 1)
      (process-node-list nl)
      (sosofo-append
       (process-node-list (node-list-first nl))
       (node-list-reduce (node-list-rest nl)
                         (lambda (sosofo snl)
                           (sosofo-append sosofo sep (process-node-list snl)))
                         (empty-sosofo)))))
