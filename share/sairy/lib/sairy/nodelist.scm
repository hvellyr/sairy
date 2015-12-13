;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

;; @doc Returns the node-list that results from appending the members of
;; @prm{nl1}, @prm{nl2}, ... @prm{nln}.  If there are no arguments, returns the
;; empty node-list.
(define-syntax node-list
  (syntax-rules ()
    ((node-list)          (empty-node-list))
    ((node-list args ...) (%node-list (list args ...)))
    ))


;; @doc Returns the mapping over @prm{nl} of the function on a node that returns
;; the value that the node exhibits for the property propname or an empty
;; node-list if the node does not exhibit a value or exhibits a null value for
;; propname.  @prm{name} can be specified in any of the ways allowed for the
;; node-property procedure.
(define (node-list-property name nl)
  (node-list-map (lambda (snl)
                   (node-property name snl default: (empty-node-list)))
                 nl))


;; @doc If @prm{nl} has no members, returns @prm{obj}, and otherwise returns the result
;; of applying node-list-reduce to 
;; @itemize
;;   @li a node-list containing all but the first member of nl,
;;   @li proc, and
;;   @li the result of applying proc to obj and the first member of nl.
;; @end{itemize}
(define (node-list-reduce nl proc obj)
  (if (node-list-empty? nl)
      obj
      (node-list-reduce (node-list-rest nl)
                        proc
                        (proc obj (node-list-first nl)))))

;; @doc For each member of @prm{nl}, applies @prm{proc} to a singleton node-list
;; containing just that member and appends the resulting node-lists.  It shall
;; be an error if @prm{proc} does not return a node-list when applied to any
;; member of @prm{nl}.
(define (node-list-map proc nl)
  (node-list-reduce nl
                    (lambda (result snl)
                      (node-list (proc snl) result))
                    (empty-node-list)))

;; @doc Returns a node-list containing just those members of @prm{nl} for which
;; @prm{predicate} applied to a singleton node-list containing just that member
;; does not return #f.  The resulting members in node-list preserve their order
;; from @prm{nl}.
(define (node-list-filter predicate nl)
  (node-list-reduce nl
                    (lambda (result snl)
                      (if (predicate snl)
                          (node-list result snl)
                          result))
                    (empty-node-list)))

;; @doc Returns a list containing, for each member of nl, a singleton node-list
;; containing just that member.
(define (node-list->list nl)
  (reverse (node-list-reduce nl (lambda (result snl)
                                  (cons snl result))
                             '())))

;; @doc Returns a node-list containing the members of @prm{nl} in reverse order.
(define (node-list-reverse nl)
  (node-list-reduce nl (lambda (result snl)
                         (node-list snl result))
                    (empty-node-list)))

;; @doc Returns a node-list containing the @prm{k}th member of nl (zero-based),
;; if there is such a member, and otherwise returns the empty node-list.
(define (node-list-ref nl k)
  (cond ((< k 0)   (empty-node-list))
        ((zero? k) (node-list-first nl))
        (else      (node-list-ref (node-list-rest nl) (- k 1)))))

;; @doc Returns the node-list comprising all but the first @prm{k} members of
;; @prm{nl}.  If @prm{nl} has @prm{k} or fewer members, returns the empty
;; node-list.
(define (node-list-tail nl k)
  (cond ((< k 0)   (empty-node-list))
        ((zero? k) nl)
        (else      (node-list-tail (node-list-rest nl) (- k 1)))))

;; @doc Returns a node-list comprising the first @prm{k} members of @prm{nl}.
;; If @prm{nl} has @prm{k} or fewer members, returns @prm{nl}.
(define (node-list-head nl k)
  (if (zero? k)
      (empty-node-list)
      (node-list (node-list-first nl)
                 (node-list-head nl (- k 1)))))

;; @doc Returns a node-list containing those members of @prm{nl} from index
;; @prm{k1} inclusive to @prm{k2} exclusive.
(define (node-list-sublist nl k1 k2)
  (node-list-head (node-list-tail nl k1) (- k2 k1)))


;; @doc Returns the @sym{attributes} property of all nodes in @prm{nl}
(define (attributes nl)
  (node-list-property 'attributes nl))


;; @doc Returns the mapping over @prm{nl} of the function that returns the
;; member of the value of the attributes property whose name is @prm{name}.
(define (attribute name nl)
  (node-list-map (lambda (snl)
                   (named-node name (attributes snl)))
                 nl))


;; @doc Returns the names of all members of @prm{nnl} in the same order as
;; @prm{nnl} as list.
(define (named-node-list-names nnl)
  (node-list-reduce nnl
                    (lambda (result snl)
                      (let ((nm (node-property 'name snl default: #f)))
                        (if (string? nm)
                            (append result (list nm))
                            result)))
                    '()))

;; @doc Returns the value of the @var{id} property of the node in @prm{osnl}.
;; If @prm{osnl} is empty or the node in @prm{osnl} does not contain an @var{id}
;; property return #f.
(define (id osnl)
  (if (node-list-empty? osnl)
      #f
      (node-property 'id osnl default: #f)))


;; @doc Returns #t if @prm{nl} contains a node equal to the member of @prm{snl},
;; and otherwise returns #f.
(define (node-list-contains? nl snl)
  (node-list-reduce nl
                    (lambda (result i)
                      (or result
                          (node-list=? snl i)))
                    #f))

;; @doc Returns a node-list which is the same as @prm{nl} except that any member
;; of @prm{nl} which is equal to a preceding member of @prm{nl} is removed.
(define (node-list-remove-duplicates nl)
  (node-list-reduce nl
                    (lambda (result snl)
                      (if (node-list-contains? result snl)
                          result
                          (node-list result snl)))
                    (empty-node-list)))

;; @doc Returns a node-list containing the union of all the arguments, which
;; shall be node-lists.  The result shall contain no duplicates.  With no
;; arguments, an empty node-list shall be returned.
(define* (node-list-union . args)
  (reduce args
          (lambda (nl1 nl2)
            (node-list-reduce nl2
                              (lambda (result snl)
                                (if (node-list-contains? result snl)
                                    result
                                    (node-list result snl)))
                              nl1))
          (empty-node-list)))

;; @doc Returns a node-list containing the intersection of all the arguments,
;; which shall be node-lists.  The result shall contain no duplicates.  With no
;; arguments, an empty node-list shall be returned.
(define (node-list-intersection . args)
  (if (null? args)
      (empty-node-list)
      (reduce (cdr args)
              (lambda (nl1 nl2)
                (node-list-reduce nl1
                                  (lambda (result snl)
                                    (if (node-list-contains? nl2 snl)
                                        (node-list result snl)
                                        result))
                                  (empty-node-list)))
              (node-list-remove-duplicates (car args)))))

;; @doc Returns a node-list containing the set difference of all the arguments,
;; which shall be node-lists.  The set difference is defined to be those members
;; of the first argument that are not members of any of the other arguments.
;; The result shall contain no duplicates.  With no arguments, an empty
;; node-list shall be returned.
(define (node-list-difference . args)
  (if (null? args)
      (empty-node-list)
      (reduce (cdr args)
              (lambda (nl1 nl2)
                (node-list-reduce nl1
                                  (lambda (result snl)
                                    (if (node-list-contains? nl2 snl)
                                        result
                                        (node-list result snl)))
                                  (empty-node-list)))
              (node-list-remove-duplicates (car args)))))

;; @doc Returns #t if, for some member of @prm{nl}, @prm{proc} does not return
;; #f when applied to a singleton node-list containing just that member, and
;; otherwise returns #f.  An implementation is allowed, but not required, to
;; signal an error if, for some member of @prm{nl}, @prm{proc} would signal an
;; error when applied to a singleton node-list containing just that member.
(define (node-list-some? proc nl)
  (node-list-reduce nl
                    (lambda (result snl)
                      (or result (proc snl)))
                    #f))

;; @doc Returns #t if, for every member of @prm{nl}, @prm{proc} does not return
;; #f when applied to a singleton node-list containing just that member, and
;; otherwise returns #f.  An implementation is allowed to signal an error if,
;; for some member of @prm{nl}, @prm{proc} would signal an error when applied to
;; a singleton node-list containing just that member.
(define (node-list-every? proc nl)
  (node-list-reduce nl
                    (lambda (result snl)
                      (and result (proc snl)))
                    #t))
