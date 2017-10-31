;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

(import (srfi 6))
(import (srfi 28))


(define default-mode-marker #f)

(define current-mode
  (make-parameter default-mode-marker))

(define %in-current-definition%
  (make-parameter #f))

(define-record-type :match-node
    (match-node _others _selector _body)
    match-node?
    (_others match-node-others match-node-set-others!)
    (_selector match-node-selector match-node-set-selector!)
    (_body match-node-body match-node-set-body!))

(define (make-match-node selector body)
  (match-node (make-hash-table) selector body))


(define (match-node-display mn0)
  (let display-node ((mn mn0)
                     (depth 0))
    (define (indent d) (make-string (* d 2) #\space))

    (if (> (hash-table-size (match-node-others mn)) 0)
        (begin
          (display (format "{")) (newline)
          (hash-table-walk (match-node-others mn)
                           (lambda (key val)
                             (display (format "~a[~s : " (indent (+ depth 1)) key))
                             (if (match-node? val)
                                 (display-node val (+ depth 1))
                                 (display "???"))
                             (display "]") (newline)))
          (display (format "~a}" (indent depth)))))
    (display (format " -> ~s" (match-node-body mn)))
    )
  (newline) )


(define *registry* (make-hash-table))

(define (rules-registry mode)
  (hash-table-ref *registry* mode
                  (lambda ()
                    (let ((new-registry (make-match-node '() #f)))
                      (hash-table-set! *registry* mode new-registry)
                      new-registry))) )


(define (register-default-rule mode . body)
  (hash-table-set! (match-node-others (rules-registry mode)) '<all>
                   (make-match-node '<default> body)))

(define (register-root-rule mode . body)
  (hash-table-set! (match-node-others (rules-registry mode)) '<root>
                   (make-match-node '<root> body)))

(define (register-text-rule mode . body)
  (hash-table-set! (match-node-others (rules-registry mode)) '<text>
                   (make-match-node '<text> body)))

(define (register-element-rule selector mode . body)
  (let loop ((gis (if (list? selector)
                      (reverse selector)
                      (list selector)))
             (parent-node (rules-registry mode)))
    (if (null? gis)
        '()
        (let* ((gi (car gis))
               (current-map (match-node-others parent-node))
               (hmn (hash-table-ref current-map gi
                                    (lambda ()
                                      (let ((new-match-node (make-match-node selector #f)))
                                        (hash-table-set! current-map gi new-match-node)
                                        new-match-node)) )))
          (if (null? (cdr gis))
              (begin
                (if (match-node-body hmn)
                    (display (format "ERROR: Rule '~s' redefined~%" selector)))
                (match-node-set-selector! hmn selector)
                (match-node-set-body! hmn body)))
          (loop (cdr gis) hmn))
        )
    ))


;; A default-element-construction-rule matches any node of class element.
(define-syntax default
  (syntax-rules ()
    ((default body ...)
     (register-default-rule (current-mode) (lambda () body ...)) )))

;; An element-construction-rule matches any node of class element that matches
;; the gi or qualified-gi.
(define-syntax element
  (syntax-rules ()
    ((element selector body ...)
     (register-element-rule 'selector (current-mode) (lambda () body ...)) )))

;; A root-construction-rule matches any document nodes
(define-syntax root
  (syntax-rules ()
    ((root body ...)
     (register-root-rule (current-mode) (lambda () body ...)) )))

;; A text-construction-rule matches any text nodes
(define-syntax text
  (syntax-rules ()
    ((text body ...)
     (register-text-rule (current-mode) (lambda () body ...)) )))

;; A construction-rule in a mode-construction-rule-group matches a node only
;; when the current processing mode is mode-name.
(define-syntax mode
  (syntax-rules ()
    ((mode mode-name body ...)
     (parameterize ((current-mode 'mode-name))
        body ...)
     )))


(define (gi-path node)
  (let loop ((nd node)
             (path '()))
    (if (null? nd)
        path
        (loop (parent nd)
              (append path (list (gi nd)))) )))


(define (selector->string sel)
  (let ((q (open-output-string)))
    (display sel q)
    (get-output-string q)))

(define (process-node-in-registry nd-path rule-node)
  (let* ((spec (let loop ((path nd-path)
                          (cur-rule-node rule-node))
                 (if (null? path)
                     (cons (match-node-selector cur-rule-node)
                           (match-node-body cur-rule-node))
                     (let ((mnd (hash-table-ref/default (match-node-others cur-rule-node)
                                                        (car path) 'no-match)))
                       (if (equal? mnd 'no-match)
                           (cons (match-node-selector cur-rule-node)
                                 (match-node-body cur-rule-node))
                           (loop (cdr path)
                                 mnd)))
                     ))) )
    (if (and (list? spec) (procedure? (cadr spec)))
        (parameterize ((%in-current-definition% (selector->string (car spec))))
                      ((cadr spec)))
        #f) ))


(define (process-node-by-path-and-mode tests-set mode nd-path)
  (let loop ((tests tests-set))
    (if (null? tests)
        (empty-sosofo)
        (let* ((test (car tests))
               (sosofo (process-node-in-registry (car test)
                                                 (rules-registry (cadr test)))))
          (if (sosofo? sosofo)
              sosofo
              (loop (cdr tests)))) )))

(define (process-element-by-path mode nd-path)
  (process-node-by-path-and-mode `((,nd-path ,mode)
                                   (,nd-path ,default-mode-marker)
                                   ((<all>) ,mode)
                                   ((<all>) ,default-mode-marker))
                                 mode nd-path))

(define (process-document mode)
  (process-node-by-path-and-mode `(((<root>) ,mode)
                                   ((<root>) ,default-mode-marker))
                                 mode
                                 '(<root>)))

(define (process-text mode)
  (process-node-by-path-and-mode `(((<text>) ,mode)
                                   ((<text>) ,default-mode-marker))
                                 mode
                                 '(<text>)))

(define (process-node mode nd)
  (case (class nd)
   ( (document) (process-document mode))
   ( (element) (process-element-by-path mode (gi-path nd)))
   ( (text) (process-text mode))
   ( else (empty-sosofo)) ))


(define-syntax with-mode
  (syntax-rules ()
    ((with-mode mode-name expr)
     (parameterize ((current-mode 'mode-name))
                   expr)
     )))


;; Returns a singleton node-list.  This node is the one currently matched.
;; (define current-node
;;   (make-parameter (empty-node-list)))

(define *current-node* (empty-node-list))

;; @doc Returns a singleton node-list.  This node is the one currently matched.
(define (current-node) *current-node*)


(define (process-current-node node)
  ;; (parameterize ((current-node node))
  ;;               (process-node (current-mode)
  ;;                             node)))
  (let ((cn (current-node)))
    (set! *current-node* node)
    (let ((res (process-node (current-mode)
                             node)))
      (set! *current-node* cn)
      res)))


;; @doc Returns the sosofo that results from appending the sosofos that result
;; from processing the members of the @prm{nl} in order.
(define (process-node-list nl)
  (node-list-reduce nl
                    (lambda (sosofo snl)
                      (sosofo-append sosofo (process-current-node snl)))
                    (empty-sosofo)))

;; @doc Returns the sosofo that results from appending the sosofos that result
;; from processing @prm{nl} in order after removing any leading and trailing
;; whitespace from leading and trailing text nodes.
(define* (process-node-list-trim nl (left?: left? #t) (right?: right? #t))
  (define (left-trim first? str)
    (if first? (string-trim str) str))

  (define (right-trim last? str)
    (if last? (string-trim-right str) str))

  (define (process-with-trim first? last? snl)
    (if (equal? (class snl) 'text)
        (literal
         (left-trim first?
                    (right-trim last?
                                (node-property 'data snl default: ""))))
        (process-current-node snl)))

  (let loop ((p nl)
             (first? #t)
             (sosofo (empty-sosofo)))
    (if (node-list-empty? p)
        sosofo
        (let ((rest (node-list-rest p)))
          (loop rest
                #f
                (sosofo-append
                 sosofo
                 (process-with-trim (and left? first?)
                                    (and right? (node-list-empty? rest))
                                    (node-list-first p))) ))
        )))

;; @doc Returns the sosofo that results from appending the sosofos that result
;; from processing in order the children of the current node after removing any
;; leading and trailing whitespace from leading and trailing text nodes.
(define (process-children-trim)
  (process-node-list-trim (children (current-node))))
