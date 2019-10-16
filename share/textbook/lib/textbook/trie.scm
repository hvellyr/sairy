;; -*-scheme-*-
;; Copyright (c) 2019 by Gregor Klinke
;; All rights reserved.

(import (srfi 9))
(import (srfi 69))

;;; private
(define-record-type :trie-empty-value
    (trie-empty-value)
    trie-empty-value?)

;;; private
(define *trie-empty-value* (trie-empty-value))

;;; private
(define-syntax make-trie-node
  (syntax-rules ()
    ((make-trie-node) (cons (make-hash-table) *trie-empty-value*))))

;;; private
(define-syntax trie-node-children
  (syntax-rules ()
    ((trie-node-children node) (car node))))

;;; private
(define-syntax trie-node-value
  (syntax-rules ()
    ((trie-node-value node) (cdr node))))

;;; private
(define-syntax trie-node-value!
  (syntax-rules ()
    ((trie-node-value! node value) (set-cdr! node value))))

;;; private
(define-record-type :trie
    (trie _root)
    trie?
    (_root trie-root))

;;; @doc Creates an empty trie object
(define (make-trie)
  (trie (make-trie-node)))


;;; private
(define (trie-find-node c key thunk)
  (let* ((key-len (string-length key)))
    (let loop ((cur-node (trie-root c))
               (idx 0))
      (if (>= idx key-len)
          cur-node
          (let* ((cur-char (string-ref key idx))
                 (next-node (hash-table-ref/default (trie-node-children cur-node)
                                                    cur-char
                                                    #f))
                 (mapped-node (if (not next-node)
                                  (thunk cur-node cur-char)
                                  next-node))
                 )
            (if (not mapped-node)
                'not-found
                (loop mapped-node (+ idx 1))))
          ))))


;;; @doc Returns the value for @prm{key} in @prm{c} or the symbol
;;; @sym{not-found} if @prm{key} is not in @prm{c}.  If @prm{key} is a prefix to
;;; some other key in @prm{c} the return value might be an
;;; @ty{trie-empty-value}.
(define (trie-ref c key)
  (let ((nd (trie-find-node c key (lambda (cur-nd cur-char) #f))))
    (if (eqv? nd 'not-found)
        nd
        (trie-node-value nd))))


;;; @doc Indicates whether @prm{key} is contained in @prm{c}.  @prm{key} is not
;;; contained if is only a prefix to another key.
(define (trie-contains c key)
  (let* ((res (trie-ref c key)))
    (not (or (eqv? res 'not-found)
             (trie-empty-value? res)))))


;;; private
(define (trie-node-fold nd pfx proc obj)
  (let* ((obj2 (if (not (trie-empty-value? (trie-node-value nd)))
                   (proc pfx (trie-node-value nd) obj)
                   obj)))
    (hash-table-fold (trie-node-children nd)
                     (lambda (key value acc)
                       (trie-node-fold value
                                       (string-append pfx (string key))
                                       proc
                                       acc))
                     obj2)))

;;; @doc Calls @prm{proc} for every association in @prm{c} with three arguments:
;;; the @em{key} of the association key, the @em{value} of the association
;;; value, and an accumulated value, @em{obj}. @em{val} is @prm{obj} for the
;;; first invocation of @prm{proc}, and for subsequent invocations of
;;; @prm{proc}, the return value of the previous invocation of @prm{proc}.  The
;;; return value returned is the return value of the last invocation of
;;; @prm{proc}.  The order in which @prm{proc} is called for different
;;; associations is unspecified.
(define (trie-fold c pfx proc obj)
  (let ((nd (trie-find-node c pfx (lambda (cur-nd cur-char) #f))))
    (if (eqv? nd 'not-found)
        obj
        (trie-node-fold nd pfx proc obj))))


;;; @doc Returns a list of all keys in @prm{c} which have a common prefix
;;; @prm{pfx} or are identical to @prm{pfx}.
(define (trie-keys c pfx)
  (trie-fold c pfx
             (lambda (key val obj)
               (append obj (list key)))
             (list)))


;;; @doc Returns a list of all values in @prm{c} which are mapped by keys with a
;;; common prefix @prm{pfx} or are identical to @prm{pfx}.
(define (trie-values c pfx)
  (trie-fold c pfx
             (lambda (key val obj)
               (append obj (list val)))
             (list)))


;;; @doc insert @prm{key} with @prm{value} into @prm{c}.  A previous inserted
;;; key @prm{key} is overwritten.
(define (trie-insert c key value)
  (let* ((nd (trie-find-node c key
                             (lambda (cur-node cur-char)
                               (let ((new-node (make-trie-node)))
                                 (hash-table-set! (trie-node-children cur-node)
                                                  cur-char
                                                  new-node)
                                 new-node)))))
    (trie-node-value! nd value)
    #t))

;;; @doc Indicates whether the trie @prm{t} is empty.
(define (trie-empty? t)
  (= (hash-table-size (trie-node-children (trie-root t))) 0))


;; ;;;----------------------------------------------------------------------------------------

;; (define t (make-trie))

;; (trie-insert t "abc" 42)
;; (trie-insert t "abx" 53)
;; (trie-insert t "abcdef" 127)


;; (display (trie-fold t "a"
;;                     (lambda (key val obj)
;;                       (display key) (display " -> ") (display val) (newline)
;;                       (append obj (list key)))
;;                     (list))) (newline)
