;; Copyright (c) 2017 by Gregor Klinke
;; All rights reserved.

(import (srfi 28))

(define (make-source def)
  (cons (format "definition of '~s'" def) #f))


(define-syntax make
  (syntax-rules ()
    ((make fo-class) (%make-fo 'fo-class (lambda () '())
                               (make-source (%in-current-definition%))))
    ((make fo-class args ...) (%make-fo 'fo-class (lambda () (list args ...))
                                        (make-source (%in-current-definition%))))
    ))


(define-syntax literal
  (syntax-rules ()
    ((literal str) (%make-fo 'literal (lambda () (list text: str))
                             (make-source (%in-current-definition%))))
    ))


(define-syntax sosofo-append
  (syntax-rules ()
    ((sosofo-append) (empty-sosofo))
    ((sosofo-append args ...) (%sosofo-append (list args ...)))
    ))


(define-record-type <style>
  (%make-style props)
  style?
  (props style-props style-props-set!))


(define-syntax style
  (syntax-rules ()
    ((style arg ...) (%make-style (lambda () (list arg ...))))
    ))


(define* (inline-space q (min: min #f) (max: max #f))
  (%make-length-spec 'inline q (if (not min) q min)
                     (if (not max) q max)
                     #f
                     1))

(define* (display-space q
                        (min: min #f) (max: max #f)
                        (conditional?: cond #f)
                        (priority: prio 1))
  (%make-length-spec 'display q (if (not min) q min)
                     (if (not max) q max)
                     cond prio))


(define-syntax region
  (syntax-rules ()
    ((region arg ...) (list arg ...))))

(define-syntax make-screen-set-model
  (syntax-rules ()
    ((make-screen-set-model regions ...) (%make-screen-set-model
                                          (lambda () (list regions ...))
                                          (make-source (%in-current-definition%))))))


(define (symbol-to-char name)
  (case name
    ((nbsp non-breaking-space) #\x00a0)
    ((narrow-non-breaking-space) #\x202f)
    ((bullet) #\x2022)
    ((triangle right-point-triangle) #\x2023)
    ((drop-shadowed-white-square) #\x274f)
    ((check-mark) #\x2713)
    ((shadowed-white-circle) #\x274d)
    ((ballot-box) #\x2610)
    ((ballot-box-with-check) #\x2611)
    ((ballot-box-with-x) #\x2612)
    ((black-right-pointing-index) #\x261b)
    ((white-right-pointing-index) #\x261e)
    ((white-circle) #\x26ac)
    ((space) #\space)
    ((endash) #\x2013)
    ((emdash) #\x2014)
    (else #\space)))

(define-syntax make-char
  (syntax-rules ()
    ((make-char name) (literal (string (symbol-to-char name))))))


(define-record-type <address>
  (%make-address local destination)
  address?
  (local address-local? address-local-set!)
  (destination address-destination address-destination-set!))

;; @doc Returns an address object representing the resource referred to by
;; @prm{url}.
(define (external-address url)
  (%make-address #f url))


;; @doc Returns an address object representing the current node.
(define (current-node-address)
  (%make-address #t (id (current-node))))

;; @doc Returns a list of an address objects refering each to the nodes in
;; @prm{nl}.
(define  (node-list-address nl)
  (node-list-reduce nl
                    (lambda (result snl)
                      (append result (%make-address #t (id snl))))
                    '()))
