;; Copyright (c) 2015 by Gregor Klinke
;; All rights reserved.

(define default-mode-marker #f)

(define current-mode
  (make-parameter default-mode-marker))

(define-record-type :match-node
    (match-node _others _body)
    match-node?
    (_others match-node-others match-node-set-others!)
    (_body match-node-body match-node-set-body!))

(define (make-match-node body)
  (match-node (make-hash-table) body))


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
                    (let ((new-registry (make-match-node #f)))
                      (hash-table-set! *registry* mode new-registry)
                      new-registry))) )


(define (register-default-rule mode . body)
  (hash-table-set! (match-node-others (rules-registry mode)) '<all> (make-match-node body)))

(define (register-root-rule mode . body)
  (hash-table-set! (match-node-others (rules-registry mode)) '<root> (make-match-node body)))

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
                                      (let ((new-match-node (make-match-node #f)))
                                        (hash-table-set! current-map gi new-match-node)
                                        new-match-node)) )))
          (if (null? (cdr gis))
              (begin
                (if (match-node-body hmn)
                    (display (format "ERROR: Rule '~s' redefined~%" selector)))
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


(define (process-node-in-registry nd-path rule-node)
  (let* ((spec (let loop ((path nd-path)
                          (cur-rule-node rule-node))
                 (if (null? path)
                     (match-node-body cur-rule-node)
                     (let ((mnd (hash-table-ref/default (match-node-others cur-rule-node)
                                                        (car path) 'no-match)))
                       (if (equal? mnd 'no-match)
                           (match-node-body cur-rule-node)
                           (loop (cdr path)
                                 mnd)))
                     ))) )
    (if (and (list? spec) (procedure? (car spec)))
        ((car spec))
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

(define (process-node mode nd)
  (case (class nd)
   ( (document) (process-document mode))
   ( (element) (process-element-by-path mode (gi-path nd)))
   ( else (empty-sosofo)) ))


;; Returns a singleton node-list.  This node is the one currently matched.
(define current-node
  (make-parameter (empty-node-list)))

;; Returns the sosofo that results from appending the sosofos that result from
;; processing the members of the @prm{nl} in order.
(define (process-node-list nl)
  (let loop ((p nl)
             (sosofo (empty-sosofo)))
    (if (node-list-empty? p)
        sosofo
        (let* ((node (node-list-head p))
               (result-sosofo (parameterize ((current-node node))
                                            (process-node (current-mode)
                                                          node))) )
          (loop (node-list-rest p)
                (sosofo-append sosofo result-sosofo))
          ))))


;; Returns the sosofo that results from appending the sosofos that result from
;; processing in order the children of the current node.
(define (process-children)
  (process-node-list (children (current-node))))


;; (hash-table-walk *registry*
;;                  (lambda (key value)
;;                    (display (format "MODE ~a~%" key))
;;                    (match-node-display value)
;;                    (display (format "------------------------------------------------~%"))))


(define-syntax dimen
  (syntax-rules ()
    ((dimen real unit) (cons real 'unit))))
