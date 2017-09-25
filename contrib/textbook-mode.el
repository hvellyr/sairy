;;; textbook-mode.el --- major mode for editing textbook files -*- coding: utf-8 -*-
;;
;; Copyright (c) 2011, 2015,2017 Gregor C. Klinke
;; All rights reserved.
;;
;; This source code is released under the BSD License.
;;
;; Derived from texinfo.el, Copyright (C) 1985, 1988, 1989, 1990, 1991,
;;   1992, 1993, 1996, 1997, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
;;   2007, 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
;;
;; Author: Gregor C. Klinke
;; Date:   2017-09-25
;; Maintainer: hvellyr
;; Keywords: TEXTBOOK, docs
;;
;; Usage:
;; Add to you .emacs file:
;;
;; (autoload 'textbook-mode "textbook-mode" "TEXTBOOK Mode" t)
;; (setq auto-mode-alist (cons '("\\.textbook\\'" . textbook-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.tb\\'" . textbook-mode) auto-mode-alist))

(eval-when-compile (require 'tex-mode) (require 'cl))
(defvar textbook-outline-heading-alist)

(defgroup textbook nil
  "TEXTBOOK Mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'docs)

;;;###autoload
(defcustom textbook-open-quote (purecopy "“")
  "String inserted by typing \\[textbook-insert-quote] to open a quotation."
  :type 'string
  :group 'textbook)

;;;###autoload
(defcustom textbook-close-quote (purecopy "”")
  "String inserted by typing \\[textbook-insert-quote] to close a quotation."
  :type 'string
  :group 'textbook)


(defcustom textbook-mode-hook nil
  "Normal hook run when entering textbook mode."
  :type 'hook
  :options '(turn-on-auto-fill flyspell-mode)
  :group 'textbook)


;;; Code:

(defvar textbook-section-list
  '(("chapter" 1)
    ("section" 2)
    ("subsection" 3)
    ("subsubsection" 4)
    ("appendix" 1)
    ("preface" 1)
    ("introduction" 1)
    ("content" 1)

    ;; mandoc
    ("synopsis" 1)
    ("seealso" 1))
  "Alist of sectioning commands and their relative level.")

;;; Syntax table

(defvar textbook-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "." st)
    (modify-syntax-entry ?\\ "." st)
    (modify-syntax-entry ?@ "\\" st)
    (modify-syntax-entry ?\^q "\\" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\' "w" st)
    st))

;; Written by Wolfgang Bangerth <zcg51122@rpool1.rus.uni-stuttgart.de>
;; To override this example, set either `imenu-generic-expression'
;; or `imenu-create-index-function'.
(defvar textbook-imenu-generic-expression
  '(("Chapters" "^@chapter[ \t]+\\(.*\\)$" 1))
  "Imenu generic expression for textbook mode.  See `imenu-generic-expression'.")

(defvar textbook-font-lock-syntactic-keywords
  '(("\\(@\\)c?\\>" (1 "<"))
    ("^\\(@\\)ignore\\>" (1 "< b"))
    ("^@end ignore\\(\n\\)" (1 "> b")))
  "Syntactic keywords to catch comment delimiters in `textbook-mode'.")

(defconst textbook-std-environments
  '("book" "manpage"
    "titlepage" "info" "license" "revision" "source" "abstract"
    "chapter" "appendix" "preface" "introduction" "section" "subsection" "subsubsection"
    "content"
    "synopsis" "seealso"
    "paragraph"
    "content"
    "definitions" "list" "enumerate" "itemize"
    "example" "verbatim" "grammar" "productions"
    "admon" "quote" "poem" "letter"
    "bibref"
    "def" "deffn" "decl"
    )
  "List of textbook environments in the standard 'book' and 'mandoc' specifications.")

(defconst textbook-environment-regexp
  (concat "^@" (regexp-opt (cons "end" textbook-std-environments) t) "\\>")
  "Regexp for environment-like textbook list commands.
Subexpression 1 is what goes into the corresponding `@end' statement.")

(defface textbook-heading
  '((t (:inherit font-lock-function-name-face)))
  "Face used for section headings in `textbook-mode'."
  :group 'textbook)
(define-obsolete-face-alias 'textbook-heading-face 'textbook-heading "22.1")
(defvar textbook-heading-face 'textbook-heading)

(defvar textbook-font-lock-keywords
  `(;; All but the first had an OVERRIDE of t.
    ;; It didn't seem to be any better, and it's slower--simon.
    ;; Robert J. Chassell <bob@gnu.org> says remove this line.
    ;;("\\$\\([^$]*\\)\\$" 1 font-lock-string-face t)
    ("@\\([a-zA-Z]+\\|[^ \t\n]\\)" 1 font-lock-keyword-face) ;commands
    ("@\\(em\\){\\([^}]+\\)" 2 'italic)
    ("@\\(b\\){\\([^}]+\\)" 2 'bold)
    ("@\\(kbd\\|key\\|url\\|uref\\){\\([^}]+\\)" 2 font-lock-string-face)
    ;; The following two groups have an OVERRIDE of `keep' because
    ;; their arguments frequently include a @@, and we don't want that
    ;; to overwrite the normal fontification of the argument.
    ("@\\(file\\|email\\){\\([^}]+\\)" 2 font-lock-string-face keep)
    ("@\\(code\\|var\\|val\\|app\\|op\\){\\([^}]+\\)"
     2 font-lock-variable-name-face keep)
    ("@\\(x?ref\\|dfn\\){\\([^}]+\\)" 2 font-lock-constant-face)
    ("@\\(=\\){\\([^}]+\\)"           2 font-lock-builtin-face)
    ("@\\(end\\|itemx?\\) +\\(.+\\)"  2 font-lock-keyword-face keep)
    ;; (,textbook-environment-regexp
    ;;  1 (textbook-clone-environment (match-beginning 1) (match-end 1)) keep)
    (,(concat "^@" (regexp-opt (mapcar 'car textbook-section-list) t)
	       ".*\n") 0 textbook-heading-face t))
  "Additional expressions to highlight in textbook mode.")

(defun textbook-clone-environment (start end)
  (let ((endp nil))
    (save-excursion
      (ignore-errors
        (goto-char start)
        (when (looking-at "end\\Sw+\\(\\sw+\\)")
          (setq endp t start (match-beginning 1) end (match-end 1)))
        (unless (get-char-property start 'text-clones)
          (if endp
              (textbook-last-unended-begin)
            (forward-word 1)
            (textbook-next-unmatched-end))
          (skip-syntax-forward "^w")
          (when (looking-at
                 (concat (regexp-quote (buffer-substring start end)) "\\>"))
            (text-clone-create start end 'spread "\\w*")))))))


;;; Keybindings

;; Mode documentation displays commands in reverse order
;; from how they are listed in the textbook-mode-map.

(defvar textbook-mode-map
  (let ((map (make-sparse-keymap)))

    ;; bindings for `textbook-mode.el'
    (define-key map "\""           'textbook-insert-quote)

    ;; AUCTeX-like bindings
    (define-key map "\e\r"		     'textbook-insert-@item)

    ;; bindings for updating nodes and menus

    (define-key map "\C-c}"        'up-list)
    (define-key map "\C-c]"        'up-list)
    (define-key map "\C-c{"		     'textbook-insert-braces)

    ;; bindings for inserting strings
    (define-key map "\C-c\C-o"     'textbook-insert-block)
    (define-key map "\C-c\C-ce"    'textbook-insert-@end)

    (define-key map "\C-c\C-c\C-s" 'textbook-insert-@b)
    (define-key map "\C-c\C-c\C-e" 'textbook-insert-@em)

    (define-key map "\C-c\C-cx"    'textbook-insert-@example)

    (define-key map "\C-c\C-cc"    'textbook-insert-@code)
    (define-key map "\C-c\C-cd"    'textbook-insert-@dfn)
    (define-key map "\C-c\C-cf"    'textbook-insert-@file)
    (define-key map "\C-c\C-ci"    'textbook-insert-@item)
    (define-key map "\C-c\C-cm"    'textbook-insert-@email)
    (define-key map "\C-c\C-cp"    'textbook-insert-@prm)
    (define-key map "\C-c\C-cr"    'textbook-insert-@ref)
    (define-key map "\C-c\C-cv"    'textbook-insert-@var)
    map))

(easy-menu-define textbook-mode-menu
  textbook-mode-map
  "Menu used for `textbook-mode'."
  '("Textbook"
    ["Insert block"	  textbook-insert-block	t]
    ))


(defun textbook-filter (section list)
  (let (res)
    (dolist (x list) (if (eq section (cadr x)) (push (car x) res)))
    res))

(defvar textbook-chapter-level-regexp
  (regexp-opt (textbook-filter 2 textbook-section-list))
  "Regular expression matching just the textbook chapter level headings.")

;;; Textbook mode

;;;###autoload
(define-derived-mode textbook-mode text-mode "textbook"
  "Major mode for editing textbook files.

  It has these extra commands:
\\{textbook-mode-map}

  Textbook is a simple markup language for producing documentations in various
output formats.  The markup resembles Scribe and GNU Texinfo markup, but is
conceptually more like SGML/XML.

  Editing commands are like text-mode except that the syntax table is set
up so expression commands skip textbook bracket groups.

TODO

  You can show the structure of a textbook file with \\[textbook-show-structure].
This command shows the structure of an textbook file by listing the lines with
the @-sign commands for @chapter, @section, and the like.  These lines are
displayed in another window called the *Occur* window.  In that window, you
can position the cursor over one of the lines and use
\\[occur-mode-goto-occurrence], to jump to the corresponding spot in the
textbook file.

  In addition, textbook mode provides commands that insert various
frequently used @-sign commands into the buffer.  You can use these
commands to save keystrokes.  And you can insert balanced braces with
\\[textbook-insert-braces] and later use the command \\[up-list] to
move forward past the closing brace.

Entering textbook mode calls the value of `text-mode-hook', followed by the
value of `textbook-mode-hook'."
  (set (make-local-variable 'page-delimiter)
       (concat "^@\\(" textbook-chapter-level-regexp "\\)\\>"))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline mode-require-final-newline)
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	(concat "\b\\|@[a-zA-Z]*[ \n]\\|" paragraph-separate))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "\b\\|@[a-zA-Z]*[ \n]\\|" paragraph-start))
  (make-local-variable 'sentence-end-base)
  (setq sentence-end-base "\\(@\\(end\\)?dots{}\\|[.?!]\\)[]\"'”)}]*")
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'fill-column)
  (setq fill-column 70)
  (make-local-variable 'comment-start)
  (setq comment-start "@c ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "@c +\\|@comment +")
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes t)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression textbook-imenu-generic-expression)
  (setq imenu-case-fold-search nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(textbook-font-lock-keywords
                             nil nil nil backward-paragraph
                             (font-lock-syntactic-keywords
                              . textbook-font-lock-syntactic-keywords)))
  (set (make-local-variable 'parse-sexp-lookup-properties) t)

  ;; Outline settings.
  (set (make-local-variable 'textbook-outline-heading-alist)
       ;; We should merge textbook-outline-heading-alist and textbook-section-list
       ;; but in the mean time, let's just generate one from the other.
       (mapcar (lambda (x) (cons (concat "@" (car x)) (cadr x)))
               textbook-section-list))
  (set (make-local-variable 'outline-regexp)
       (concat (regexp-opt (mapcar 'car textbook-outline-heading-alist) t) "\\>"))

  ;; Prevent filling certain lines, in addition to those specified by the
  ;; user.
  (let ((prevent-filling "^@\\(def\\|example\\|verbatim\\)"))
    (set (make-local-variable 'auto-fill-inhibit-regexp)
         (if (null auto-fill-inhibit-regexp)
             prevent-filling
           (concat auto-fill-inhibit-regexp "\\|" prevent-filling)))))



;;; Insert string commands

(defvar textbook-block-default "example")

(define-skeleton textbook-insert-block
  "Create a matching pair @<cmd> .. @end <cmd> at point.
Puts point on a blank line between them."
  (setq textbook-block-default
        (completing-read (format "Block name [%s]: " textbook-block-default)
                         textbook-std-environments
                         nil nil nil nil textbook-block-default))
  \n "@" str \n _ \n "@end " str \n)

(defun textbook-inside-macro-p (macro &optional bound)
  "Non-nil if inside a macro matching the regexp MACRO."
  (condition-case nil
      (save-excursion
        (save-restriction
          (narrow-to-region bound (point))
          (while (progn
                   (up-list -1)
                   (not (condition-case nil
                            (save-excursion
                              (backward-sexp 1)
                              (looking-at macro))
                          (scan-error nil)))))
          t))
    (scan-error nil)))

(defun textbook-inside-env-p (env &optional bound)
  "Non-nil if inside an environment matching the regexp @ENV."
  (save-excursion
    (and (re-search-backward (concat "@\\(end\\s +\\)?" env) bound t)
         (not (match-end 1)))))

(defvar textbook-enable-quote-macros "@\\(code\\)\\>")
(defvar textbook-enable-quote-envs '("example\\>"))
(defun textbook-insert-quote (&optional arg)
  "Insert the appropriate quote mark for textbook.
Usually inserts the value of `textbook-open-quote' (normally “) or
`textbook-close-quote' (normally ”), depending on the context.  With prefix
argument or inside @code or @example, inserts a plain \"."
  (interactive "*P")
  (let ((top (or (save-excursion (re-search-backward "@node\\>" nil t))
                 (point-min))))
    (if (or arg
            (= (preceding-char) ?\\)
            (save-excursion
              ;; Might be near the start of a (narrowed) buffer.
              (ignore-errors (backward-char (length textbook-open-quote)))
              (when (or (looking-at textbook-open-quote)
                        (looking-at textbook-close-quote))
                (delete-char (length textbook-open-quote))
                t))
            (textbook-inside-macro-p textbook-enable-quote-macros top)
            (let ((in-env nil))
              (dolist (env textbook-enable-quote-envs in-env)
                (if (textbook-inside-env-p env top)
                    (setq in-env t)))))
        (self-insert-command (prefix-numeric-value arg))
      (insert
       (if (or (bobp)
               (memq (char-syntax (preceding-char)) '(?\( ?> ?\s)))
           textbook-open-quote
         textbook-close-quote)))))

;; The following textbook-insert-@end command not only inserts a SPC after the
;; @end, but tries to find out what belongs there.  It is not very smart:
;; it does not understand nested lists.

(defun textbook-last-unended-begin ()
  (while (and (re-search-backward textbook-environment-regexp)
              (looking-at "@end"))
    (textbook-last-unended-begin)))

(defun textbook-next-unmatched-end ()
  (while (and (re-search-forward textbook-environment-regexp)
              (save-excursion
                (goto-char (match-beginning 0))
                (not (looking-at "@end"))))
    (textbook-next-unmatched-end)))

(defun textbook-insert-@end ()
  "Insert the matching `@end' for the last Textbook command that needs one."
  (interactive)
  (let ((string
         (ignore-errors
           (save-excursion
             (textbook-last-unended-begin)
             (match-string 1)))))
    (insert "@end ")
    (if string (insert string "\n"))))

;; The following insert commands accept a prefix arg N, which is the number
;; of words (actually s-exprs) that should be surrounded by braces.  Thus
;; you can first paste a variable name into an .textbook buffer, then say C-u 1
;; C-c C-c v at the beginning of the just pasted variable name to put
;; @var{...} *around* the variable name.  Operate on previous word or words
;; with negative arg.

;; These commands use textbook-insert-@-with-arg
(defun textbook-insert-@-with-arg (string &optional arg)
  (if arg
      (progn
        (setq arg (prefix-numeric-value arg))
        (if (< arg 0)
            (progn
              (skip-chars-backward " \t\n\r\f")
              (save-excursion
                (forward-sexp arg)
                (insert "@" string "{"))
              (insert "}"))
          (skip-chars-forward " \t\n\r\f")
          (insert "@" string "{")
          (forward-sexp arg)
          (insert "}")))
    (insert "@" string "{}")
    (backward-char)))

(defun textbook-insert-braces ()
  "Make a pair of braces and be poised to type inside of them.
Use \\[up-list] to move forward out of the braces."
  (interactive)
  (insert "{}")
  (backward-char))

(defun textbook-insert-@code (&optional arg)
  "Insert a `@code{...}' command in a textbook buffer.
A numeric argument says how many words the braces should surround.  The
default is not to surround any existing words with the braces."
  (interactive "P")
  (textbook-insert-@-with-arg "code" arg))

(defun textbook-insert-@dfn (&optional arg)
  "Insert a `@dfn{...}' command in a textbook buffer.
A numeric argument says how many words the braces should surround.  The
default is not to surround any existing words with the braces."
  (interactive "P")
  (textbook-insert-@-with-arg "dfn" arg))

(defun textbook-insert-@email (&optional arg)
  "Insert a `@email{...}' command in a textbook buffer.
A numeric argument says how many words the braces should surround.  The
default is not to surround any existing words with the braces."
  (interactive "P")
  (textbook-insert-@-with-arg "email" arg))

(defun textbook-insert-@em (&optional arg)
  "Insert a `@emph{...}' command in a textbook buffer.
A numeric argument says how many words the braces should surround.  The
default is not to surround any existing words with the braces."
  (interactive "P")
  (textbook-insert-@-with-arg "emph" arg))

(defun textbook-insert-@example ()
  "Insert the string `@example' in a textbook buffer."
  (interactive)
  (insert "@example\n"))

(defun textbook-insert-@file (&optional arg)
  "Insert a `@file{...}' command in a textbook buffer.
A numeric argument says how many words the braces should surround.  The
default is not to surround any existing words with the braces."
  (interactive "P")
  (textbook-insert-@-with-arg "file" arg))

(defun textbook-insert-@item ()
  "Insert the string `@item' in a textbook buffer.
If in a table defined by @table, follow said string with a space.
Otherwise, follow with a newline."
  (interactive)
  (insert "@item"))

(defun textbook-insert-@b (&optional arg)
  "Insert a `@strong{...}' command in a textbook buffer.
A numeric argument says how many words the braces should surround.  The
default is not to surround any existing words with the braces."
  (interactive "P")
  (textbook-insert-@-with-arg "strong" arg))

(defun textbook-insert-@var (&optional arg)
  "Insert a `@var{}' command in a textbook buffer.
A numeric argument says how many words the braces should surround.  The
default is not to surround any existing words with the braces."
  (interactive "P")
  (textbook-insert-@-with-arg "var" arg))

(defun textbook-insert-@prm (&optional arg)
  "Insert a `@var{}' command in a textbook buffer.
A numeric argument says how many words the braces should surround.  The
default is not to surround any existing words with the braces."
  (interactive "P")
  (textbook-insert-@-with-arg "var" arg))

(defun textbook-insert-@ref (&optional arg)
  "Insert a `@ref{}' command in a textbook buffer.
A numeric argument says how many words the braces should surround.  The
default is not to surround any existing words with the braces."
  (interactive "P")
  (textbook-insert-@-with-arg "ref" arg))


(provide 'textbook)

;;; textbook-model.el ends here
