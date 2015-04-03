;;; tstyle-mode.el --- major mode for editing textbook style files -*- coding: utf-8 -*-
;;
;; Copyright (c) 2015 Gregor C. Klinke
;; All rights reserved.
;;
;; This source code is released under the BSD License.
;;
;; Derived from the DSSSL definition as part of Emacs default scheme mode
;; Copyright (C) 1986-1988, 1997-1998, 2001-2013 Free Software
;; Foundation, Inc.
;;
;; Author: Gregor C. Klinke
;; Date:   2015-03-29
;; Maintainer: hvellyr
;; Keywords: TSTYLE, docs, stylesheets, textbook
;;
;; Usage:
;; Add to you .emacs file:
;;
;; (autoload 'tstyle-mode "tstyle-mode" "TSTYLE Mode" t)
;; (setq auto-mode-alist (cons '("\\.tstyle\\'" . tstyle-mode) auto-mode-alist))

(require 'scheme)

(defcustom tstyle-mode-hook nil
  "Normal hook run when entering `tstyle-mode'.
See `run-hooks'."
  :type 'hook
  :group 'scheme)


(defvar tstyle-imenu-generic-expression
  '(("Defines"
     "^(define\\s-+(?\\(\\sw+\\)" 1)
    ("Modes"
     "^\\s-*(mode\\s-+\\(\\(\\sw\\|\\s-\\)+\\)" 1)
    ("Elements"
     ;; (element foo ...) or (element (foo bar ...) ...)
     ;; Fixme: Perhaps it should do `root'.
     "^\\s-*(element\\s-+(?\\(\\(\\sw\\|\\s-\\)+\\))?" 1)
    ("Declarations"
     "^(declare\\(-\\sw+\\)+\\>\\s-+\\(\\sw+\\)" 2))
  "Imenu generic expression for TSTYLE mode.  See `imenu-generic-expression'.")


;;;###autoload
(define-derived-mode tstyle-mode scheme-mode "TSTYLE"
  "Major mode for editing TSTYLE style sheets.
Editing commands are similar to those of `lisp-mode'.

TSTYLE is a style sheets language similar to DSSSL.

Commands:

Delete converts tabs to spaces as it moves back.  Blank lines
separate paragraphs.  Semicolons start comments.
\\{scheme-mode-map} Entering this mode runs the hooks
`scheme-mode-hook' and then `tstyle-mode-hook'."
  (set (make-local-variable 'page-delimiter) "^;;;")
  (setq font-lock-defaults '(tstyle-font-lock-keywords
			     nil t (("+-*/.<>=?$%_&~^:" . "w"))
			     beginning-of-defun
			     (font-lock-mark-block-function . mark-defun)))
  (set (make-local-variable 'imenu-case-fold-search) nil)
  (setq imenu-generic-expression tstyle-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
       '(("+-*/.<>=?$%_&~^:" . "w"))))

;; Extra syntax for TSTYLE.  This isn't separated from Scheme, but shouldn't cause
;; much trouble in scheme-mode.
(put 'default 'scheme-indent-function 1)
(put 'root 'scheme-indent-function 1)
(put 'element 'scheme-indent-function 1)
(put 'text 'scheme-indent-function 1)
(put 'mode 'scheme-indent-function 1)
(put 'make 'scheme-indent-function 1)
(put 'style 'scheme-indent-function 1)
(put 'with-mode 'scheme-indent-function 1)


(defvar tstyle-builtin-functions
  (regexp-opt
   '(
     "absolute-first-element-sibling?"
     "absolute-first-sibling?"
     "absolute-last-element-sibling?"
     "absolute-last-sibling?"
     "ancestors"
     "attribute"
     "attributes"
     "children"
     "class"
     "current-mode"
     "current-node"
     "descendants"
     "dimen"
     "dimen?"
     "empty-node-list"
     "empty-sosofo"
     "follow"
     "gi"
     "grove-root"
     "literal"
     "match-element?"
     "named-node"
     "named-node-list-names"
     "node-list"
     "node-list->list"
     "node-list-empty?"
     "node-list-first"
     "node-list-length"
     "node-list-map"
     "node-list-property"
     "node-list-reduce"
     "node-list-ref"
     "node-list-rest"
     "node-list-reverse"
     "node-list-sublist"
     "node-list-tail"
     "node-list?"
     "node-property"
     "parent"
     "preced"
     "process-children"
     "process-children-trim"
     "process-node-list"
     "register-default-rule"
     "register-element-rule"
     "register-root-rule"
     "select-elements"
     "siblings"
     "sosofo-append"
     "sosofo?"
     )))

(defvar tstyle-font-lock-keywords
  (eval-when-compile
    (list
     ;; Similar to Scheme
     (list "(\\(define\\(-\\w+\\)?\\)\\>[ 	]*\\\((?\\)\\(\\sw+\\)\\>"
           '(1 font-lock-keyword-face)
           '(4 font-lock-function-name-face))
     (cons
      (concat "(\\("
              ;; (make-regexp '("case" "cond" "else" "if" "lambda"
              ;; "let" "let*" "letrec" "and" "or" "map" "with-mode"))
              "and\\|c\\(ase\\|ond\\)\\|else\\|if\\|"
              "l\\(ambda\\|et\\(\\|*\\|rec\\)\\)\\|map\\|or\\|with-mode"
              "\\)\\>")
      1)
     ;; TSTYLE syntax
     '("(\\(element\\|mode\\|declare-\\w+\\)\\>[ 	]*\\(\\sw+\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face))
     '("(\\(element\\)\\>[ 	]*(\\(\\S)+\\))"
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face))
     '("(\\(root\\|default\\|text\\)\\>"
       (1 font-lock-keyword-face))
     '("(\\(make\\)\\>[ 	]*\\(\\sw+\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
     (list (concat "(\\(" tstyle-builtin-functions "\\)\\>")
           '(1 font-lock-builtin-face))
     '("\\<\\sw+:\\>" . font-lock-constant-face) ; trailing `:' c.f. scheme
     )
    )
  "Default expressions to highlight in TSTYLE mode.")

(provide 'tstyle-mode)
