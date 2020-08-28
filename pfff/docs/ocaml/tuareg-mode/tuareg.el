;;; tuareg.el --- OCaml mode for Emacs.  -*- coding: utf-8 -*-

;; Copyright (C) 1997-2006 Albert Cohen, all rights reserved.
;; Copyright (C) 2009-2010 Jane Street Holding, LLC.
;; Licensed under the GNU General Public License.

;; Author: Albert Cohen <Albert.Cohen@inria.fr>
;;	Sam Steingold <sds@gnu.org>
;;	Christophe Troestler <Christophe.Troestler@umons.ac.be>
;;	Till Varoquaux <till@pps.jussieu.fr>
;;	Sean McLaughlin <seanmcl@gmail.com>
;;	Stefan Monnier <monnier@iro.umontreal.ca>
;; Created: 8 Jan 1997
;; Version: 2.0.6
;; Package-Requires: ((caml "3.12.0.1"))
;; Keywords: ocaml languages
;; URL: http://forge.ocamlcore.org/projects/tuareg/
;; EmacsWiki: TuaregMode

;;; Commentary:
;; Description:
;; Tuareg helps editing OCaml code, to highlight important parts of
;; the code, to run an OCaml toplevel, and to run the OCaml debugger
;; within Emacs.

;; Installation:
;; If you have permissions to the local `site-lisp' directory, you
;; only have to copy `tuareg.el' and `ocamldebug.el' Otherwise, copy
;; `tuareg.el' and `ocamldebug.el' to a local directory and add the
;; following line to your `.emacs'
;;
;; (add-to-list 'load-path "DIR")


;;; Usage:
;; Tuareg allows you to run batch OCaml compilations from Emacs (using
;; M-x compile) and browse the errors (C-x `). Typing C-x ` sets the
;; point at the beginning of the erroneous program fragment, and the
;; mark at the end.  Under Emacs, the program fragment is temporarily
;; hilighted.
;;
;; M-x tuareg-run-ocaml starts an OCaml toplevel with input and output in
;; an Emacs buffer named `*ocaml-toplevel*. This gives you the full
;; power of Emacs to edit the input to the OCaml toplevel. This mode is
;; based on comint so you get all the usual comint features, including
;; command history. A hook named `tuareg-interactive-mode-hook' may be
;; used for customization.
;;
;; Typing C-c C-e in a buffer in tuareg mode sends the current phrase
;; (containing the point) to the OCaml toplevel, and evaluates it.  If
;; you type one of these commands before M-x tuareg-run-ocaml, the
;; toplevel will be started automatically.
;;
;; M-x ocamldebug FILE starts the OCaml debugger ocamldebug on the
;; executable FILE, with input and output in an Emacs buffer named
;; *ocamldebug-FILE*.  It is similar to April 1996 version, with minor
;; changes to support XEmacs, Tuareg and OCaml. Furthermore, package
;; `thingatpt' is not required any more.

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;;; Code:

(eval-when-compile (require 'cl))
(require 'easymenu)

(defconst tuareg-mode-revision
  (eval-when-compile
    (with-temp-buffer
      (cond ((file-directory-p ".hg")
	     (call-process "hg" nil t nil "id" "-i" "--debug"))
	    ((file-directory-p ".svn")
             (let ((process-environment
                    (cons "LANG=C" process-environment)))
               (shell-command "svn info | grep Revision: | sed 's/Revision: //'" t)))
	    ((file-directory-p ".bzr")
	     (shell-command "bzr log -l -1 | grep revno:" t)))
      (unless (zerop (buffer-size))
        (buffer-substring-no-properties
         (point-min) (1- (point-max))))))
  "Tuareg revision from the control system used.")

(defconst tuareg-mode-version
  (let ((version "Tuareg Version 2.0.6"))
    (if (null tuareg-mode-revision)
	version
      (concat version " (" tuareg-mode-revision ")")
      ))
  "         Copyright (C) 1997-2006 Albert Cohen, all rights reserved.
         Copyright (C) 2009-2010 Jane Street Holding, LLC.
         Copyright (C) 2011- Stefan Monnier & Christophe Troestler
         Copying is covered by the GNU General Public License.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Compatibility functions

(defun tuareg-editing-ls3 ()
  "Tell whether we are editing Lucid Synchrone syntax."
  (string-match "\\.ls\\'" (or buffer-file-name (buffer-name))))

(defun tuareg-editing-ocamllex ()
  "Tell whether we are editing OCamlLex syntax."
  (string-match "\\.mll\\'" (or buffer-file-name (buffer-name))))

(defalias 'tuareg-match-string
  (if (fboundp 'match-string-no-properties)
      'match-string-no-properties
    'match-string))

(or (fboundp 'read-shell-command)
    (defun read-shell-command  (prompt &optional initial-input history)
      "Read a string from the minibuffer, using `shell-command-history'."
      (read-from-minibuffer prompt initial-input nil nil
                            (or history 'shell-command-history))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Import types and help features

(defvar tuareg-with-caml-mode-p
  (and (require 'caml-types nil t) (require 'caml-help nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       User customizable variables

;; Use the standard `customize' interface or `tuareg-mode-hook' to
;; Configure these variables

(require 'custom)

(defgroup tuareg nil
  "Support for the OCaml language."
  :group 'languages)

;; Comments

(defcustom tuareg-indent-leading-comments t
  "*If true, indent leading comment lines (starting with `(*') like others."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-indent-comments t
  "*If true, automatically align multi-line comments."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-comment-end-extra-indent 0
  "*How many spaces to indent a leading comment end `*)'.
If you expect comments to be indented like
        (*
          ...
         *)
even without leading `*', use `tuareg-comment-end-extra-indent' = 1."
  :group 'tuareg
  :type '(radio :extra-offset 8
                :format "%{Comment End Extra Indent%}:
   Comment alignment:\n%v"
                (const :tag "align with `(' in comment opening" 0)
                (const :tag "align with `*' in comment opening" 1)
                (integer :tag "custom alignment" 0)))

(defcustom tuareg-support-leading-star-comments t
  "*Enable automatic intentation of comments of the form
        (*
         * ...
         *)
Documentation comments (** *) are not concerned by this variable
unless `tuareg-leading-star-in-doc' is also set.

If you do not set this variable and still expect comments to be
indented like
        (*
          ...
         *)
\(without leading `*'), set `tuareg-comment-end-extra-indent' to 1."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-leading-star-in-doc nil
  "*Enable automatic intentation of documentation comments of the form
        (**
         * ...
         *)"
  :group 'tuareg :type 'boolean)

;; Indentation defaults

(defcustom tuareg-default-indent 2
  "*Default indentation.

Global indentation variable (large values may lead to indentation overflows).
When no governing keyword is found, this value is used to indent the line
if it has to."
  :group 'tuareg :type 'integer)

(defcustom tuareg-support-camllight nil
  "*If true, handle Caml Light character syntax (incompatible with labels)."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (when (boundp 'tuareg-mode-syntax-table)
           (modify-syntax-entry ?` (if val "\"" ".")
                                tuareg-mode-syntax-table))))

(defcustom tuareg-support-metaocaml nil
  "*If true, handle MetaOCaml syntax."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (ignore-errors
           (tuareg-make-indentation-regexps)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when (derived-mode-p 'tuareg-mode 'tuareg-interactive-mode)
                 (tuareg-install-font-lock)))))))

(defcustom tuareg-let-always-indent t
  "*If true, enforce indentation is at least `tuareg-let-indent' after a `let'.

As an example, set it to nil when you have `tuareg-with-indent' set to 0,
and you want `let x = match ... with' and `match ... with' indent the
same way."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-pipe-extra-unindent tuareg-default-indent
  "*Extra backward indent for OCaml lines starting with the `|' operator.

It is NOT the variable controlling the indentation of the `|' itself:
this value is automatically added to `function', `with', `parse' and
some cases of `type' keywords to leave enough space for `|' backward
indentation.

For example, setting this variable to 0 leads to the following indentation:
  match ... with
    X -> ...
    | Y -> ...
    | Z -> ...

To modify the indentation of lines lead by `|' you need to modify the
indentation variables for `with', `function', and possibly
for `type' as well. For example, setting them to 0 (and leaving
`tuareg-pipe-extra-unindent' to its default value) yields:
  match ... with
    X -> ...
  | Y -> ...
  | Z -> ..."
  :group 'tuareg :type 'integer)

(defcustom tuareg-class-indent tuareg-default-indent
  "*How many spaces to indent from a `class' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-sig-struct-align t
  "*Align `sig' and `struct' keywords with `module'."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-sig-struct-indent tuareg-default-indent
  "*How many spaces to indent from a `sig' or `struct' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-method-indent tuareg-default-indent
  "*How many spaces to indent from a `method' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-begin-indent tuareg-default-indent
  "*How many spaces to indent from a `begin' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-for-while-indent tuareg-default-indent
  "*How many spaces to indent from a `for' or `while' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-do-indent tuareg-default-indent
  "*How many spaces to indent from a `do' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-fun-indent tuareg-default-indent
  "*How many spaces to indent from a `fun' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-function-indent 0 ;tuareg-default-indent
  "*How many spaces to indent from a `function' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-if-then-else-indent tuareg-default-indent
  "*How many spaces to indent from an `if', `then' or `else' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-let-indent tuareg-default-indent
  "*How many spaces to indent from a `let' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-in-indent 0 ; tuareg-default-indent
  "*How many spaces to indent from a `in' keyword.
Upstream <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
recommends 0, and this is what we default to since 2.0.1
instead of the historical `tuareg-default-indent'."
  :group 'tuareg :type 'integer)

(defcustom tuareg-match-indent tuareg-default-indent
  "*How many spaces to indent from a `match' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-try-indent tuareg-default-indent
  "*How many spaces to indent from a `try' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-with-indent 0
  "*How many spaces to indent from a `with' keyword.
The examples at <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
show the '|' is aligned with 'match', thus 0 is the default value."
  :group 'tuareg :type 'integer)

(defcustom tuareg-match-clause-indent 1
  "*How many spaces to indent a clause after a pattern match `| ... ->'.
To respect <http://caml.inria.fr/resources/doc/guides/guidelines.en.html>
the default is 1.")

(defcustom tuareg-match-when-indent (+ 4 tuareg-match-clause-indent)
  "*How many spaces from `|' to indent `when' in a pattern match
   | patt
        when cond ->
      clause")

(defcustom tuareg-type-indent 0 ;tuareg-default-indent
  "*How many spaces to indent from a `type' keyword."
  :group 'tuareg :type 'integer)

(defcustom tuareg-val-indent tuareg-default-indent
  "*How many spaces to indent from a `val' keyword."
  :group 'tuareg :type 'integer)

;; Automatic indentation
;; Using abbrev-mode and electric keys

(defcustom tuareg-use-abbrev-mode t
  "*Non-nil means electrically indent lines starting with leading keywords.
Leading keywords are such as `end', `done', `else' etc.
It makes use of `abbrev-mode'.

Many people find electric keywords irritating, so you can disable them by
setting this variable to nil."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (derived-mode-p 'tuareg-mode)
               (abbrev-mode (if val 1 -1)))))))

(defcustom tuareg-electric-indent t
  "*Non-nil means electrically indent lines starting with `|', `)', `]' or `}'.

Many people find electric keys irritating, so you can disable them by
setting this variable to nil."
  :group 'tuareg :type 'boolean)
(when (fboundp 'electric-indent-mode)
  (make-obsolete-variable 'tuareg-electric-indent
                          'electric-indent-mode "Emacs-24.1"))

(defcustom tuareg-electric-close-vector t
  "*Non-nil means electrically insert `|' before a vector-closing `]' or
`>' before an object-closing `}'.

Many people find electric keys irritating, so you can disable them by
setting this variable to nil. You should probably have this on,
though, if you also have `tuareg-electric-indent' on."
  :group 'tuareg :type 'boolean)

;; Tuareg-Interactive
;; Configure via `tuareg-mode-hook'

(defcustom tuareg-interactive-scroll-to-bottom-on-output nil
  "*Controls when to scroll to the bottom of the interactive buffer
upon evaluating an expression.

See `comint-scroll-to-bottom-on-output' for details."
  :group 'tuareg :type 'boolean
  :set (lambda (var val)
         (set-default var val)
         (when (boundp 'comint-scroll-to-bottom-on-output)
           (dolist (buf (buffer-list))
             (with-current-buffer buf
               (when (derived-mode-p 'tuareg-interactive-mode)
                 (set (make-local-variable 'comint-scroll-to-bottom-on-output)
                      val)))))))

(defcustom tuareg-skip-after-eval-phrase t
  "*Non-nil means skip to the end of the phrase after evaluation in the
Caml toplevel."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-read-only-input nil
  "*Non-nil means input sent to the OCaml toplevel is read-only."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-echo-phrase t
  "*Non-nil means echo phrases in the toplevel buffer when sending
them to the OCaml toplevel."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-input-font-lock t
  "*Non nil means Font-Lock for toplevel input phrases."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-output-font-lock t
  "*Non nil means Font-Lock for toplevel output messages."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-interactive-error-font-lock t
  "*Non nil means Font-Lock for toplevel error messages."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-display-buffer-on-eval t
  "*Non nil means pop up the OCaml toplevel when evaluating code."
  :group 'tuareg :type 'boolean)

(defcustom tuareg-manual-url
  "http://pauillac.inria.fr/ocaml/htmlman/index.html"
  "*URL to the OCaml reference manual."
  :group 'tuareg :type 'string)

(defcustom tuareg-browser 'browse-url
  "*Name of function that displays the OCaml reference manual.
Valid names are `browse-url', `browse-url-firefox', etc."
  :group 'tuareg)

(defcustom tuareg-library-path "/usr/local/lib/ocaml/"
  "*Path to the OCaml library."
  :group 'tuareg :type 'string)

(defcustom tuareg-definitions-max-items 30
  "*Maximum number of items a definitions menu can contain."
  :group 'tuareg :type 'integer)

(defvar tuareg-options-list
  '(("Automatic indentation of leading keywords" . 'tuareg-use-abbrev-mode)
    ("Automatic indentation of ), ] and }" . 'tuareg-electric-indent)
    ("Automatic matching of [| and {<" . 'tuareg-electric-close-vector)
    "---"
    ("Indent body of comments" . 'tuareg-indent-comments)
    ("Indent first line of comments" . 'tuareg-indent-leading-comments)
    ("Leading-`*' comment style" . 'tuareg-support-leading-star-comments))
  "*List of menu-configurable Tuareg options.")

(defvar tuareg-interactive-options-list
  '(("Skip phrase after evaluation" . 'tuareg-skip-after-eval-phrase)
    ("Echo phrase in interactive buffer" . 'tuareg-interactive-echo-phrase)
    "---"
    ("Font-lock interactive input" . 'tuareg-interactive-input-font-lock)
    ("Font-lock interactive output" . 'tuareg-interactive-output-font-lock)
    ("Font-lock interactive error" . 'tuareg-interactive-error-font-lock)
    "---"
    ("Read only input" . 'tuareg-interactive-read-only-input))
  "*List of menu-configurable Tuareg options.")

(defvar tuareg-interactive-program "ocaml"
  "*Default program name for invoking an OCaml toplevel from Emacs.")
;; Could be interesting to have this variable buffer-local
;;   (e.g., ocaml vs. metaocaml buffers)
;; (make-variable-buffer-local 'tuareg-interactive-program)

(eval-and-compile
  (defconst tuareg-use-syntax-ppss (fboundp 'syntax-ppss)
    "*If nil, use our own parsing and caching."))

(defgroup tuareg-faces nil
  "Special faces for the Tuareg mode."
  :group 'tuareg)

(defconst tuareg-faces-inherit-p
  (and (boundp 'face-attribute-name-alist)
       (assq :inherit face-attribute-name-alist)))

(defface tuareg-font-lock-governing-face
  '((((background light)) (:foreground "blue" :bold t))
    (t (:foreground "orange" :bold t)))
  "Face description for governing/leading keywords."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-governing-face
  'tuareg-font-lock-governing-face)

(defface tuareg-font-lock-multistage-face
  '((((background light))
     (:foreground "darkblue" :background "lightgray" :bold t))
    (t (:foreground "steelblue" :background "darkgray" :bold t)))
  "Face description for MetaOCaml staging operators."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-multistage-face
  'tuareg-font-lock-multistage-face)

(defface tuareg-font-lock-operator-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face description for all operators."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-operator-face
  'tuareg-font-lock-operator-face)

(defface tuareg-font-lock-error-face
  '((t (:foreground "yellow" :background "red" :bold t)))
  "Face description for all errors reported to the source."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-error-face
  'tuareg-font-lock-error-face)

(defface tuareg-font-lock-interactive-output-face
  '((((background light))
     (:foreground "blue4"))
    (t (:foreground "cyan")))
  "Face description for all toplevel outputs."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-interactive-output-face
  'tuareg-font-lock-interactive-output-face)

(defface tuareg-font-lock-interactive-error-face
  (if tuareg-faces-inherit-p
      '((t :inherit font-lock-warning-face))
    '((((background light)) (:foreground "red3"))
      (t (:foreground "red2"))))
  "Face description for all toplevel errors."
  :group 'tuareg-faces)
(defvar tuareg-font-lock-interactive-error-face
  'tuareg-font-lock-interactive-error-face)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Support definitions

(defun tuareg-leading-star-p ()
  (and tuareg-support-leading-star-comments
       (save-excursion ; this function does not make sense outside of a comment
         (tuareg-beginning-of-literal-or-comment)
         (and (or tuareg-leading-star-in-doc
                  (not (looking-at "(\\*[Tt][Ee][Xx]\\|(\\*\\*")))
              (progn
                (forward-line 1)
                (back-to-indentation)
                (looking-at "\\*[^)]"))))))

(defun tuareg-auto-fill-insert-leading-star (&optional leading-star)
  (let ((point-leading-comment (looking-at "(\\*")) (return-leading nil))
    (save-excursion
      (back-to-indentation)
      (when tuareg-electric-indent
        (when (and (tuareg-in-comment-p)
                   (or leading-star
                       (tuareg-leading-star-p)))
          (unless (looking-at "(?\\*")
            (insert-before-markers "* "))
          (setq return-leading t))
        (unless point-leading-comment
          ;; Use optional argument to break recursion
          (tuareg-indent-command t))))
    return-leading))

(unless (fboundp 'comment-normalize-vars)
  (defun tuareg-auto-fill-function ()
    (unless (tuareg-in-literal-p)
      (let ((leading-star
             (and (not (char-equal ?\n last-command-event))
                  (tuareg-auto-fill-insert-leading-star))))
        (do-auto-fill)
        (unless (char-equal ?\n last-command-event)
          (tuareg-auto-fill-insert-leading-star leading-star))))))

;; these two functions are different from the standard
;; in that they do NOT signal errors beginning-of-buffer and end-of-buffer
(defun tuareg-forward-char (&optional step)
  (if step (goto-char (+ (point) step))
    (goto-char (1+ (point)))))

(defun tuareg-backward-char (&optional step)
  (if step (goto-char (- (point) step))
    (goto-char (1- (point)))))

(defun tuareg-in-indentation-p ()
  "Return non-nil if all chars between beginning of line and point are blanks."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defvar tuareg-cache-stop (point-min))
(make-variable-buffer-local 'tuareg-cache-stop)
(defvar tuareg-cache nil)
(make-variable-buffer-local 'tuareg-cache)
(defvar tuareg-cache-local nil)
(make-variable-buffer-local 'tuareg-cache-local)
(defvar tuareg-cache-last-local nil)
(make-variable-buffer-local 'tuareg-cache-last-local)
(defvar tuareg-last-loc (cons nil nil))

;; PPSS definitions
(defun tuareg-ppss-in-literal-or-comment () (error "tuareg uses PPSS"))
(defun tuareg-ppss-fontify (beg end) (error "tuareg uses PPSS"))
(defun tuareg-ppss-in-literal-p ()
  "Return non-nil if point is inside an OCaml literal."
  (nth 3 (syntax-ppss)))
(defun tuareg-ppss-in-comment-p ()
  "Return non-nil if point is inside or right before an OCaml comment."
  (or (nth 4 (syntax-ppss))
      (looking-at "[ \t]*(\\*")))
(defun tuareg-ppss-in-literal-or-comment-p ()
  "Return non-nil if point is inside an OCaml literal or comment."
  (nth 8 (syntax-ppss)))
(defun tuareg-ppss-beginning-of-literal-or-comment ()
  "Skips to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (goto-char (or (nth 8 (syntax-ppss)) (point))))
(defun tuareg-ppss-beginning-of-literal-or-comment-fast ()
  (goto-char (or (nth 8 (syntax-ppss)) (point-min))))
;; FIXME: not clear if moving out of a string/comment counts as 1 or no.
(defalias 'tuareg-backward-up-list 'backward-up-list)

;; non-PPSS definitions
(defun tuareg-!ppss-in-literal-p ()
  "Return non-nil if point is inside an OCaml literal."
  (car (tuareg-in-literal-or-comment)))
(defun tuareg-!ppss-in-comment-p ()
  "Return non-nil if point is inside an OCaml comment."
  (cdr (tuareg-in-literal-or-comment)))
(defun tuareg-!ppss-in-literal-or-comment-p ()
  "Return non-nil if point is inside an OCaml literal or comment."
  (tuareg-in-literal-or-comment)
  (or (car tuareg-last-loc) (cdr tuareg-last-loc)))
(defun tuareg-!ppss-in-literal-or-comment ()
  "Return the pair `((tuareg-in-literal-p) . (tuareg-in-comment-p))'."
  (if (and (<= (point) tuareg-cache-stop) tuareg-cache)
      (progn
        (if (or (not tuareg-cache-local) (not tuareg-cache-last-local)
                (and (>= (point) (caar tuareg-cache-last-local))))
            (setq tuareg-cache-local tuareg-cache))
        (while (and tuareg-cache-local (< (point) (caar tuareg-cache-local)))
          (setq tuareg-cache-last-local tuareg-cache-local
                tuareg-cache-local (cdr tuareg-cache-local)))
        (setq tuareg-last-loc
              (if tuareg-cache-local
                  (cons (eq (cadar tuareg-cache-local) 'b)
                        (> (cddar tuareg-cache-local) 0))
                  (cons nil nil))))
    (let ((flag t) (op (point)) (mp (min (point) (1- (point-max))))
          (balance 0) (end-of-comment nil))
      (while (and tuareg-cache (<= tuareg-cache-stop (caar tuareg-cache)))
        (setq tuareg-cache (cdr tuareg-cache)))
      (if tuareg-cache
          (if (eq (cadar tuareg-cache) 'b)
              (progn
                (setq tuareg-cache-stop (1- (caar tuareg-cache)))
                (goto-char tuareg-cache-stop)
                (setq balance (cddar tuareg-cache))
                (setq tuareg-cache (cdr tuareg-cache)))
            (setq balance (cddar tuareg-cache))
            (setq tuareg-cache-stop (caar tuareg-cache))
            (goto-char tuareg-cache-stop)
            (skip-chars-forward "("))
          (goto-char (point-min)))
      (skip-chars-backward "\\\\*")
      (while flag
        (if end-of-comment (setq balance 0 end-of-comment nil))
        (skip-chars-forward "^\\\\'`\"(\\*")
        (cond
          ((looking-at "\\\\")
           (tuareg-forward-char 2))
          ((looking-at "'\\([^\n\\']\\|\\\\[^ \t\n][^ \t\n]?[^ \t\n]?\\)'")
           (setq tuareg-cache (cons (cons (1+ (point)) (cons 'b balance))
                                    tuareg-cache))
           (goto-char (match-end 0))
           (setq tuareg-cache (cons (cons (point) (cons 'e balance))
                                    tuareg-cache)))
          ((and
            tuareg-support-camllight
            (looking-at "`\\([^\n\\']\\|\\\\[^ \t\n][^ \t\n]?[^ \t\n]?\\)`"))
           (setq tuareg-cache (cons (cons (1+ (point)) (cons 'b balance))
                                    tuareg-cache))
           (goto-char (match-end 0))
           (setq tuareg-cache (cons (cons (point) (cons 'e balance))
                                    tuareg-cache)))
          ((looking-at "\"")
           (tuareg-forward-char)
           (setq tuareg-cache (cons (cons (point) (cons 'b balance))
                                    tuareg-cache))
           (skip-chars-forward "^\\\\\"")
           (while (looking-at "\\\\")
             (tuareg-forward-char 2) (skip-chars-forward "^\\\\\""))
           (tuareg-forward-char)
           (setq tuareg-cache (cons (cons (point) (cons 'e balance))
                                    tuareg-cache)))
          ((looking-at "(\\*")
           (setq balance (1+ balance))
           (setq tuareg-cache (cons (cons (point) (cons nil balance))
                                    tuareg-cache))
           (tuareg-forward-char 2))
          ((looking-at "\\*)")
           (tuareg-forward-char 2)
           (if (> balance 1)
               (progn
                 (setq balance (1- balance))
                 (setq tuareg-cache (cons (cons (point) (cons nil balance))
                                          tuareg-cache)))
               (setq end-of-comment t)
               (setq tuareg-cache (cons (cons (point) (cons nil 0))
                                        tuareg-cache))))
          (t (tuareg-forward-char)))
        (setq flag (<= (point) mp)))
      (setq tuareg-cache-local tuareg-cache
            tuareg-cache-stop (point))
      (goto-char op)
      (if tuareg-cache (tuareg-in-literal-or-comment)
          (setq tuareg-last-loc (cons nil nil))
          tuareg-last-loc))))
(defun tuareg-!ppss-beginning-of-literal-or-comment ()
  "Skips to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (when (tuareg-in-literal-or-comment-p)
    (tuareg-beginning-of-literal-or-comment-fast)))

(defun tuareg-!ppss-beginning-of-literal-or-comment-fast ()
  (while (and tuareg-cache-local
              (or (eq 'b (cadar tuareg-cache-local))
                  (> (cddar tuareg-cache-local) 0)))
    (setq tuareg-cache-last-local tuareg-cache-local
          tuareg-cache-local (cdr tuareg-cache-local)))
  (if tuareg-cache-last-local
      (goto-char (caar tuareg-cache-last-local))
    (goto-char (point-min)))
  (when (eq 'b (cadar tuareg-cache-last-local)) (tuareg-backward-char)))

(defun tuareg-!ppss-backward-up-list ()
  "Safe up-list regarding comments, literals and errors."
  (let ((balance 1) (op (point)) (oc nil))
    (tuareg-in-literal-or-comment)
    (while (and (> (point) (point-min)) (> balance 0))
      (setq oc (if tuareg-cache-local (caar tuareg-cache-local) (point-min)))
      (condition-case nil (up-list -1) (error (goto-char (point-min))))
      (if (>= (point) oc) (setq balance (1- balance))
        (goto-char op)
        (skip-chars-backward "^[]{}()") (tuareg-backward-char)
        (cond ((tuareg-in-literal-or-comment-p)
               (tuareg-beginning-of-literal-or-comment-fast))
              ((looking-at "[[{(]")
               (setq balance (1- balance)))
              ((looking-at "[]})]")
               (setq balance (1+ balance)))))
      (setq op (point)))))

(defalias 'tuareg-in-literal-or-comment
  ;; FIXME: These eval-and-compile have no effect.  Maybe eval-when-compile
  ;; was intended?
  (eval-and-compile (if tuareg-use-syntax-ppss
                        'tuareg-ppss-in-literal-or-comment
                      'tuareg-!ppss-in-literal-or-comment)))
(defalias 'tuareg-fontify
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-fontify
                          'tuareg-!ppss-fontify)))
(defalias 'tuareg-in-literal-p
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-in-literal-p
                          'tuareg-!ppss-in-literal-p)))
(defalias 'tuareg-in-comment-p
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-in-comment-p
                          'tuareg-!ppss-in-comment-p)))
(defalias 'tuareg-in-literal-or-comment-p
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-in-literal-or-comment-p
                          'tuareg-!ppss-in-literal-or-comment-p)))
(defalias 'tuareg-beginning-of-literal-or-comment
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-beginning-of-literal-or-comment
                          'tuareg-!ppss-beginning-of-literal-or-comment)))
(defalias 'tuareg-beginning-of-literal-or-comment-fast
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'tuareg-ppss-beginning-of-literal-or-comment-fast
                          'tuareg-!ppss-beginning-of-literal-or-comment-fast)))
(defalias 'tuareg-backward-up-list
    ;; FIXME: not clear if moving out of a string/comment counts as 1 or no.
    (eval-and-compile (if tuareg-use-syntax-ppss
                          'backward-up-list
                          'tuareg-!ppss-backward-up-list)))

(defun tuareg-false-=-p ()
  "Is the underlying `=' the first/second letter of an operator?"
  (or (memq (preceding-char) '(?: ?> ?< ?=))
      (char-equal ?= (char-after (1+ (point))))))

(defun tuareg-at-phrase-break-p ()
  "Is the underlying `;' a phrase break?"
  (and (char-equal ?\; (following-char))
       (or (and (not (eobp))
                (char-equal ?\; (char-after (1+ (point)))))
           (char-equal ?\; (preceding-char)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Font-lock in Emacs

;; Originally by Stefan Monnier

(defcustom tuareg-font-lock-symbols nil
  "*Display fun and -> and such using symbols in fonts.
This may sound like a neat trick, but note that it can change the
alignment and can thus lead to surprises."
  :group 'tuareg :type 'boolean)

(defvar tuareg-font-lock-symbols-alist
  (cond ((fboundp 'decode-char) ;; or a unicode font.
         `(("fun" . ,(decode-char 'ucs 955))
           ("sqrt" . ,(decode-char 'ucs 8730))
           ("not" . ,(decode-char 'ucs 172))
           ("&&" . ,(decode-char 'ucs 8743)); 'LOGICAL AND' (U+2227)
           ("or" . ,(decode-char 'ucs 8744)); 'LOGICAL OR' (U+2228)
           ("||" . ,(decode-char 'ucs 8744))
           ("[|" . ,(decode-char 'ucs 12314)) ;; 〚
           ("|]" . ,(decode-char 'ucs 12315)) ;; 〛
           ("*." . ,(decode-char 'ucs 215))
           ("/." . ,(decode-char 'ucs 247))
           ("->" . ,(decode-char 'ucs 8594))
           ("<-" . ,(decode-char 'ucs 8592))
           ("<=" . ,(decode-char 'ucs 8804))
           (">=" . ,(decode-char 'ucs 8805))
           ("<>" . ,(decode-char 'ucs 8800))
           ("==" . ,(decode-char 'ucs 8801))
           ("!=" . ,(decode-char 'ucs 8802))
           ("<=>" . ,(decode-char 'ucs 8660))
           (":=" . ,(decode-char 'ucs 8656))
           ("infinity" . ,(decode-char 'ucs 8734))
           ;; Some greek letters for type parameters.
           ("'a" . ,(decode-char 'ucs 945))
           ("'b" . ,(decode-char 'ucs 946))
           ("'c" . ,(decode-char 'ucs 947))
           ("'d" . ,(decode-char 'ucs 948))
           ("'e" . ,(decode-char 'ucs 949))
           ("'f" . ,(decode-char 'ucs 966))
           ("'i" . ,(decode-char 'ucs 953))
           ("'k" . ,(decode-char 'ucs 954))
           ("'m" . ,(decode-char 'ucs 956))
           ("'n" . ,(decode-char 'ucs 957))
           ("'o" . ,(decode-char 'ucs 969))
           ("'p" . ,(decode-char 'ucs 960))
           ("'r" . ,(decode-char 'ucs 961))
           ("'s" . ,(decode-char 'ucs 963))
           ("'t" . ,(decode-char 'ucs 964))
           ("'x" . ,(decode-char 'ucs 958))))
	((and (fboundp 'make-char) (fboundp 'charsetp) (charsetp 'symbol))
	 `(("fun" . ,(make-char 'symbol 108))
	   ("sqrt" . ,(make-char 'symbol 214))
           ("not" . ,(make-char 'symbol 216))
           ("&&" . ,(make-char 'symbol 217))
           ("or" . ,(make-char 'symbol 218))
           ("||" . ,(make-char 'symbol 218))
           ("*." . ,(make-char 'symbol 183))
           ("/." . ,(make-char 'symbol 184))
           ("<=" . ,(make-char 'symbol 163))
           ("<-" . ,(make-char 'symbol 172))
           ("->" . ,(make-char 'symbol 174))
           (">=" . ,(make-char 'symbol 179))
           ("<>" . ,(make-char 'symbol 185))
           ("==" . ,(make-char 'symbol 186))
           ("<=>" . ,(make-char 'symbol 219))
           (":=" . ,(make-char 'symbol 220))
           ("=>" . ,(make-char 'symbol 222))
           ("infinity" . ,(make-char 'symbol 165))
           ;; Some greek letters for type parameters.
           ("'a" . ,(make-char 'symbol 97))
           ("'b" . ,(make-char 'symbol 98))
           ("'c" . ,(make-char 'symbol 103)) ; sic! 99 is chi, 103 is gamma
           ("'d" . ,(make-char 'symbol 100))
           ("'e" . ,(make-char 'symbol 101))
           ("'f" . ,(make-char 'symbol 102))
           ("'i" . ,(make-char 'symbol 105))
           ("'k" . ,(make-char 'symbol 107))
           ("'m" . ,(make-char 'symbol 109))
           ("'n" . ,(make-char 'symbol 110))
           ("'o" . ,(make-char 'symbol 111))
           ("'p" . ,(make-char 'symbol 112))
           ("'r" . ,(make-char 'symbol 114))
           ("'s" . ,(make-char 'symbol 115))
           ("'t" . ,(make-char 'symbol 116))
           ("'x" . ,(make-char 'symbol 120))))))

(defun tuareg-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((mbegin (match-beginning 0))
         (mend (match-end 0))
         (syntax (char-syntax (char-after mbegin))))
    (if (or (eq (char-syntax (or (char-before mbegin) ?\ )) syntax)
            (eq (char-syntax (or (char-after mend) ?\ )) syntax)
            (memq (get-text-property mbegin 'face)
                  '(tuareg-doc-face font-lock-string-face
                    font-lock-comment-face)))
        ;; No composition for you. Let's actually remove any composition
        ;;   we may have added earlier and which is now incorrect.
        (remove-text-properties mbegin mend '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region mbegin mend (cdr (assoc (match-string 0) alist)))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun tuareg-font-lock-symbols-keywords ()
  (when (fboundp 'compose-region)
    (let ((alist nil))
      (dolist (x tuareg-font-lock-symbols-alist)
        (when (and (if (fboundp 'char-displayable-p)
                       (char-displayable-p (cdr x))
                     t)
                   (not (assoc (car x) alist))) ; not yet in alist.
          (push x alist)))
      (when alist
        `((,(regexp-opt (mapcar 'car alist) t)
           (0 (tuareg-font-lock-compose-symbol ',alist))))))))

(defvar tuareg-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?. "_" st)     ;Make qualified names a single symbol.
    (modify-syntax-entry ?? ". p" st)
    (modify-syntax-entry ?~ ". p" st)
    ;; See http://caml.inria.fr/pub/docs/manual-ocaml/lex.html.
    (dolist (c '(?! ?$ ?% ?& ?+ ?- ?/ ?: ?< ?= ?> ?@ ?^ ?|))
      (modify-syntax-entry c "." st))
    (modify-syntax-entry ?' "_" st) ; ' is part of symbols (for primes).
    (modify-syntax-entry
     ;; ` is punctuation or character delimiter (Caml Light compatibility).
     ?` (if tuareg-support-camllight "\"" ".") st)
    (modify-syntax-entry ?\" "\"" st) ; " is a string delimiter
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?*  ". 23" st)
    (condition-case nil
        (progn
          (modify-syntax-entry ?\( "()1n" st)
          (modify-syntax-entry ?\) ")(4n" st))
      (error               ;XEmacs signals an error instead of ignoring `n'.
       (modify-syntax-entry ?\( "()1" st)
       (modify-syntax-entry ?\) ")(4" st)))
    st)
  "Syntax table in use in Tuareg mode buffers.")

(defmacro tuareg-with-internal-syntax (&rest body)
  `(progn
     ;; Switch to a modified internal syntax.
     (modify-syntax-entry ?. "w" tuareg-mode-syntax-table)
     (modify-syntax-entry ?' "w" tuareg-mode-syntax-table)
     (modify-syntax-entry ?_ "w" tuareg-mode-syntax-table)
     (unwind-protect (progn ,@body)
       ;; Switch back to the interactive syntax.
       (modify-syntax-entry ?. "_" tuareg-mode-syntax-table)
       (modify-syntax-entry ?' "_" tuareg-mode-syntax-table)
       (modify-syntax-entry ?_ "_" tuareg-mode-syntax-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  Font-Lock

(defvar tuareg-doc-face 'font-lock-doc-face)

(unless tuareg-use-syntax-ppss

  (defun tuareg-fontify-buffer ()
    (font-lock-default-fontify-buffer)
    (tuareg-fontify (point-min) (point-max)))

  (defun tuareg-fontify-region (begin end &optional verbose)
    (font-lock-default-fontify-region begin end verbose)
    (tuareg-fontify begin end))

  (defun tuareg-fontify (begin end)
    (when (eq major-mode 'tuareg-mode)
      (save-excursion
       (tuareg-with-internal-syntax

        (let ((case-fold-search nil)
              (modified (buffer-modified-p))) ; Emacs hack (see below)
          (goto-char begin)
          (setq begin (line-beginning-position))
          (goto-char (1- end))
          (end-of-line)
          ;; Dirty hack to trick `font-lock-default-unfontify-region'
          (forward-line 2)
          (setq end (point))

          (while (> end begin)
            (goto-char (1- end))
            (tuareg-in-literal-or-comment)
            (cond
              ((cdr tuareg-last-loc)
               (tuareg-beginning-of-literal-or-comment)
               (put-text-property (max begin (point)) end 'face
                                  (if (looking-at
                                       "(\\*[Tt][Ee][Xx]\\|(\\*\\*[^*]")
                                      tuareg-doc-face
                                      'font-lock-comment-face))
               (setq end (1- (point))))
              ((car tuareg-last-loc)
               (tuareg-beginning-of-literal-or-comment)
               (put-text-property (max begin (point)) end 'face
                                  'font-lock-string-face)
               (setq end (point)))
              (t (while (and tuareg-cache-local
                             (or (> (caar tuareg-cache-local) end)
                                 (eq 'b (cadar tuareg-cache-local))))
                   (setq tuareg-cache-local (cdr tuareg-cache-local)))
                 (setq end (if tuareg-cache-local
                               (caar tuareg-cache-local) begin)))))
          (unless modified (set-buffer-modified-p nil)))
        ))))
  ) ;; end tuareg-use-syntax-ppss

(defconst tuareg-font-lock-syntactic-keywords
  ;; Char constants start with ' but ' can also appear in identifiers.
  ;; Beware not to match things like '*)hel' or '"hel' since the first '
  ;; might be inside a string or comment.
  ;; Note: for compatibility with Emacs<23, we use "\\<" rather than "\\_<",
  ;; which depends on tuareg-font-lock-syntax turning all "_" into "w".
  '(("\\<\\('\\)\\([^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
     (1 '(7)) (3 '(7)))))

(defvar syntax-propertize-function)
(when (eval-when-compile (fboundp 'syntax-propertize-rules))
  (defun tuareg-syntax-propertize (start end)
    (goto-char start)
    (tuareg--syntax-quotation end)
    (funcall
     (syntax-propertize-rules
      ;; When we see a '"', knowing whether it's a literal char (as opposed to
      ;; the end of a string followed by the beginning of a literal char)
      ;; requires checking syntax-ppss as in:
      ;; ("\\_<\\('\"'\\)"
      ;;  (1 (unless (nth 3 (save-excursion (syntax-ppss (match-beginning 0))))
      ;;       (string-to-syntax "\""))))
      ;; Not sure if it's worth the trouble since adding a space between the
      ;; string and the literal char is easy enough and is the usual
      ;; style anyway.
      ;; For all other cases we don't need to check syntax-ppss because, if the
      ;; first quote is within a string (or comment), the whole match is within
      ;; the string (or comment), so the syntax-properties don't hurt.
      ;;
      ;; Note: we can't just use "\\<" here because syntax-propertize is also
      ;; used outside of font-lock.
      ("\\_<\\('\\)\\(?:[^'\\\n]\\|\\\\.[^\\'\n \")]*\\)\\('\\)"
       (1 "\"") (2 "\""))
      ("\\(<\\)\\(?:<\\S.\\|:[[:alpha:]]+<\\)"
       (1 (prog1 "|" (tuareg--syntax-quotation end))))
      )
     start end)))

(defun tuareg--syntax-quotation (end)
  (when (eq t (nth 3 (syntax-ppss)))
    ;; We're indeed inside a quotation.
    (when (re-search-forward ">>" end t)
      (put-text-property (1- (point)) (point)
                         'syntax-table (string-to-syntax "|")))))

(defun tuareg-font-lock-syntactic-face-function (state)
  (if (nth 3 state)
      (if (eq t (nth 3 state))
          font-lock-preprocessor-face font-lock-string-face)
    (let ((start (nth 8 state)))
      (if (and (> (point-max) (+ start 2))
               (eq (char-after (+ start 2)) ?*)
               (not (eq (char-after (+ start 3)) ?*)))
          ;; This is a documentation comment
          tuareg-doc-face
        font-lock-comment-face))))

;; Initially empty, set in `tuareg-install-font-lock'
(defvar tuareg-font-lock-keywords ()
  "Font-Lock patterns for Tuareg mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Keymap

(defvar tuareg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "|" 'tuareg-electric-pipe)
    (define-key map ")" 'tuareg-electric-rp)
    (define-key map "}" 'tuareg-electric-rc)
    (define-key map "]" 'tuareg-electric-rb)
    (define-key map "\M-q" 'tuareg-indent-phrase)
    (define-key map "\C-c\C-q" 'tuareg-indent-phrase)
    ;; Don't bother: it's the global default anyway.
    ;;(define-key map "\M-\C-\\" 'indent-region)
    (define-key map "\C-c\C-a" 'tuareg-find-alternate-file)
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-xnd" 'tuareg-narrow-to-phrase)
    (define-key map "\M-\C-x" 'tuareg-eval-phrase)
    (define-key map [remap newline-and-indent] 'tuareg-newline-and-indent)
    (define-key map "\C-x\C-e" 'tuareg-eval-phrase)
    (define-key map "\C-c\C-e" 'tuareg-eval-phrase)
    (define-key map "\C-c\C-r" 'tuareg-eval-region)
    (define-key map "\C-c\C-b" 'tuareg-eval-buffer)
    (define-key map "\C-c\C-s" 'tuareg-run-ocaml)
    (define-key map "\C-c\C-i" 'tuareg-interrupt-ocaml)
    (define-key map "\C-c\C-k" 'tuareg-kill-ocaml)
    (define-key map "\C-c\C-n" 'tuareg-next-phrase)
    (define-key map "\C-c\C-p" 'tuareg-previous-phrase)
    (define-key map [(backspace)] 'backward-delete-char-untabify)
    (define-key map [(control c) (home)]
      'tuareg-move-inside-module-or-class-opening)
    (define-key map [(control c) (control down)] 'tuareg-next-phrase)
    (define-key map [(control c) (control up)] 'tuareg-previous-phrase)
    (define-key map [(meta control down)]  'tuareg-next-phrase)
    (define-key map [(meta control up)] 'tuareg-previous-phrase)
    (define-key map [(meta control n)]  'tuareg-next-phrase)
    (define-key map [(meta control p)] 'tuareg-previous-phrase)
    (define-key map [(meta control h)] 'tuareg-mark-phrase)
    (define-key map "\C-c`" 'tuareg-interactive-next-error-source)
    (define-key map "\C-c?" 'tuareg-interactive-next-error-source)
    (define-key map "\C-c.c" 'tuareg-insert-class-form)
    (define-key map "\C-c.b" 'tuareg-insert-begin-form)
    (define-key map "\C-c.f" 'tuareg-insert-for-form)
    (define-key map "\C-c.w" 'tuareg-insert-while-form)
    (define-key map "\C-c.i" 'tuareg-insert-if-form)
    (define-key map "\C-c.l" 'tuareg-insert-let-form)
    (define-key map "\C-c.m" 'tuareg-insert-match-form)
    (define-key map "\C-c.t" 'tuareg-insert-try-form)
    (when tuareg-with-caml-mode-p
      ;; Trigger caml-types
      (define-key map [?\C-c ?\C-t] 'caml-types-show-type)  ; "type"
      (define-key map [?\C-c ?\C-f] 'caml-types-show-call)  ; "function"
      (define-key map [?\C-c ?\C-l] 'caml-types-show-ident) ; "let"
      ;; To prevent misbehavior in case of error during exploration.
      (define-key map [?\C-c mouse-1] 'caml-types-mouse-ignore)
      (define-key map [?\C-c down-mouse-1] 'caml-types-explore)
      ;; Trigger caml-help
      (define-key map [?\C-c ?i] 'ocaml-add-path)
      (define-key map [?\C-c ?\[] 'ocaml-open-module)
      (define-key map [?\C-c ?\]] 'ocaml-close-module)
      (define-key map [?\C-c ?h] 'caml-help)
      (define-key map [?\C-c ?\t] 'tuareg-complete))
    map)
  "Keymap used in Tuareg mode.")

(defconst tuareg-font-lock-syntax
  ;; Note: as a general rule, changing syntax-table during font-lock
  ;; is a potential problem for syntax-ppss.
  `((?_ . "w") (?' . "w")
    ,@(unless tuareg-use-syntax-ppss
        '((?` . ".") (?\" . ".") (?\( . ".") (?\) . ".") (?* . "."))))
  "Syntax changes for Font-Lock.")

(defvar tuareg-electric-indent-keywords
  '("module" "class" "functor" "object" "type" "val" "inherit"
    "include" "virtual" "constraint" "exception" "external" "open"
    "method" "and" "initializer" "to" "downto" "do" "done" "else"
    "begin" "end" "let" "in" "then" "with"))

(defvar tuareg-mode-abbrev-table ()
  "Abbrev table used for Tuareg mode buffers.")
(if tuareg-mode-abbrev-table ()
  (define-abbrev-table 'tuareg-mode-abbrev-table
    (mapcar (lambda (keyword)
              `(,keyword ,keyword tuareg-abbrev-hook nil t))
            tuareg-electric-indent-keywords)))

(defun tuareg--electric-indent-predicate (char)
  "Check whether we should auto-indent.
For use on `electric-indent-functions'."
  (save-excursion
    (forward-char -1) ;; Go before the inserted char.
    (let ((syntax (char-syntax char)))
      (if (tuareg-in-indentation-p)
          (or (eq char ?|) (eq syntax ?\)))
        (or (case char
              (?\) (char-equal ?* (preceding-char)))
              (?\} (and (char-equal ?> (preceding-char))
                        (progn (tuareg-backward-char)
                               (tuareg-in-indentation-p))))
              (?\] (and (char-equal ?| (preceding-char))
                        (progn (tuareg-backward-char)
                               (tuareg-in-indentation-p)))))
            (and tuareg-use-abbrev-mode  ;; Misnomer, eh?
                 (not (eq syntax ?w))
                 (let ((end (point)))
                   (skip-syntax-backward "w_")
                   (member (buffer-substring (point) end)
                           tuareg-electric-indent-keywords))
                                           (tuareg-in-indentation-p)))))))

;;; SMIE

;; TODO:
;; - Obey tuareg-*-indent customization variables.
;; - Fix use of tuareg-indent-command in tuareg-auto-fill-insert-leading-star.
;; - Use it by default (when possible).
;; - Move the old indentation code to a separate file.

(defvar tuareg-use-smie nil)            ;Not used by default yet.

(require 'smie nil 'noerror)

(defconst tuareg-smie-grammar
  ;; Problems:
  ;; - "let D in E" expression vs "let D" declaration.  This is solved
  ;;   by making the lexer return "d-let" for the second case.
  ;; - FIXME: SMIE assumes that concatenation binds tighter than
  ;;   everything else, whereas OCaml gives tighter precedence to ".".
  ;; - "x : t1; (y : (t2 -> t3)); z : t4" but
  ;;   "when (x1; x2) -> (z1; z2)".  We solve this by distinguishing
  ;;   the two kinds of arrows, using "t->" for the type arrow.
  ;; - The "with" in modules's "with type" has different precedence.
  ;; - Big problem with "if...then": because of SMIE's transitivity of the
  ;;   precedence relation, we can't properly parse both "if A then B; C" and
  ;;   "if A then let x = E in B; C else D" (IOW I think a non-transitive OPG
  ;;   could do it).  We could try and fix the problem in the lexer, but it's
  ;;   far from obvious how (we'd probably end up having to pre-parse the text
  ;;   in the lexer to decide which kind of "if" and "then" we're looking
  ;;   at).  A good solution could be found maybe if SMIE let us disambiguate
  ;;   lexemes late, i.e. at a time where we have access to the relevant parse
  ;;   stack.  Or maybe by allowing smie-grammar to use a non-transitive
  ;;   precedence relation.  But until that happens, we will live with an
  ;;   incorrect parse, and instead we try to patch up the result with ad-hoc
  ;;   hacks in tuareg-smie-rules.
  ;; - The "<module-type> with <mod-constraints>" syntax introduces many
  ;;   conflicts:
  ;;      "... with module M = A with module B = C"
  ;;   vs      "... module M = A with module B = C"
  ;;   In the first, the second "with" should either have the first "with" as
  ;;   sibling, or have some earlier construct as parent, whereas in the second
  ;;   the "with" should have the first "=" (or maybe the first "module", tho
  ;;   that would not correspond to the actual language syntax and would
  ;;   probably break other cases) as parent.  Other problems in this
  ;;   mod-constraints syntax: we need a precedence along the lines of
  ;;   "with" < "and" < "module/type", whereas the rest of the syntax wants
  ;;   "module/type" < "and" < "with", so basically all the keywords involved
  ;;   in mod-constraints need to be handled specially in the lexer :-(
  ;; - and then some...
  (when (fboundp 'smie-prec2->grammar)
    (let ((bnfprec2
           (smie-bnf->prec2
            '((decls (decls "type" decls) (decls "d-let" decls)
                     (decls "and" decls) (decls ";;" decls)
                     (decls "exception" decls)
                     (decls "module" decls)
                     (decls "class" decls)
                     (decls "val" decls) (decls "external" decls)
                     (decls "open" decls) (decls "include" decls)
                     (exception)
                     (def)
                     ;; Hack: at the top-level, a "let D in E" can appear in
                     ;; decls as well, but the lexer classifies it as "d-let",
                     ;; so we need to make sure that "d-let D in E" doesn't
                     ;; end up matching the "in" with some far away thingy.
                     (def-in-exp))
              (def-in-exp (defs "in" exp))
              (def (var "d=" exp) (id "d=" datatype) (id "d=" module))
              (idtype (id ":" type))
              (var (id) ("m-type" var) ("rec" var) (idtype)
                   ("l-module" var) ("l-class" var))
              (exception (id "of" type))
              (datatype ("{" typefields "}") (typebranches)
                        (typebranches "with" id))
              (typebranches (typebranches "|" typebranches) (id "of" type))
              (typefields (typefields ";" typefields) (idtype))
              (type (type "*…" type) (type "t->" type)
                    ;; ("<" ... ">") ;; FIXME!
                    (type "as" id))
              (id)
              (module ("struct" decls "end")
                      ("sig" decls "end")
                      ("functor" id "->" module)
                      (module "m-with" mod-constraints))
              (simpledef (id "c=" type))
              (mod-constraints (mod-constraints "m-and" mod-constraints)
                               ("w-type" simpledef)
                               ("w-module" simpledef))
              ;; http://caml.inria.fr/pub/docs/manual-ocaml/expr.html
              ;; exp1 is "all exps except for `if exp then'".
              (exp1 ("begin" exp "end")
                    ("(" exp:type ")")
                    ("[|" exp "|]")
                    ("{" fields "}")
                    ("if" exp "then" exp1 "else" exp1)
                    ;; ("if" exp "then" exp)
                    ("while" exp "do" exp "done")
                    ("for" forbounds "do" exp "done")
                    (exp1 ";" exp1)
                    ("match" exp "with" branches)
                    ("function" branches)
                    ("fun" patterns "->" exp1)
                    ("try" exp "with" branches)
                    ("let" defs "in" exp1)
                    ("object" class-body "end")
                    ("(" exp:>type ")")
                    ("{<" fields ">}"))
              ;; Like `exp' but additionally allow if-then without else.
              (exp (exp1) ("if" exp "then" exp))
              (forbounds (iddef "to" exp) (iddef "downto" exp))
              (defs (def) (defs "and" defs) ("l-open" id))
              (exp:>type (exp:type ":>" type))
              (exp:type (exp)) ;; (exp ":" type)
              (fields (fields1) (exp "with" fields1))
              (fields1 (fields1 ";" fields1) (iddef))
              (iddef (id "f=" exp1))
              (branches (branches "|" branches) (branch))
              (branch (patterns "->" exp1))
              (patterns (pattern) (pattern "when" exp1))
              (pattern (id) (pattern "as" id) (pattern "," pattern))
              (class-body (class-body "inherit" class-body)
                          (class-body "method" class-body)
                          (class-body "val" class-body)
                          (class-body "constraint" class-body)
                          (class-field))
              (class-field (exp) ("mutable" idtype)
                           ("virtual" idtype) ("private" idtype))
              ;; We get cyclic dependencies between ; and | because things like
              ;; "branches | branches" implies that "; > |" whereas "exp ; exp"
              ;; implies "| > ;" and while those two do not directly conflict
              ;; because they're constraints on precedences of different sides,
              ;; they do introduce a cycle later on because those operators are
              ;; declared associative, which adds a constraint that both sides
              ;; must be of equal precedence.  So we declare here a dummy rule
              ;; to force a direct conflict, that we can later resolve with
              ;; explicit precedence rules.
              (foo1 (foo1 ";" foo1) (foo1 "|" foo1))
              ;; "mutable x : int ; y : int".
              (foo2 ("mutable" id) (foo2 ";" foo2))
              )
            ;; Type precedence rules.
            ;; http://caml.inria.fr/pub/docs/manual-ocaml/types.html
            '((nonassoc "as") (assoc "t->") (assoc "*…"))
            ;; Pattern precedence rules.
            ;; http://caml.inria.fr/pub/docs/manual-ocaml/patterns.html
            ;; Note that we don't include "|" because its precedence collides
            ;; with the one of the | used between branches and resolving the
            ;; conflict in the lexer is not worth the trouble.
            '((nonassoc "as") (assoc ",") (assoc "::"))
            ;; Resolve "{a=(1;b=2)}" vs "{(a=1);(b=2)}".
            '((nonassoc ";") (nonassoc "f="))
            ;; Resolve "(function a -> b) | c -> d".
            '((nonassoc "function") (nonassoc "|"))
            ;; Resolve "when (function a -> b) -> c".
            '((nonassoc "function") (nonassoc "->"))
            ;; Resolve ambiguity "(let d in e2); e3" vs "let d in (e2; e3)".
            '((nonassoc "in" "match" "->" "with") (nonassoc ";"))
            ;; Resolve "(if a then b else c);d" vs "if a then b else (c; d)".
            '((nonassoc ";") (nonassoc "else")) ;; ("else" > ";")
            ;; Resolve "match e1 with a → (match e2 with b → e3 | c → e4)"
            ;;      vs "match e1 with a → (match e2 with b → e3) | c → e4"
            '((nonassoc "with") (nonassoc "|"))
            ;; Resolve "functor A -> (M with MC)".
            '((nonassoc "->") (nonassoc "m-with"))
            ;; Resolve the conflicts caused by "when" and by SMIE's assumption
            ;; that all non-terminals can match the empty string.
            '((nonassoc "with") (nonassoc "->")) ; "when (match a with) -> e"
            '((nonassoc "|") (nonassoc "->")) ; "when (match a with a|b) -> e"
            ;; Fix up conflict between (decls "and" decls) and (defs "in" exp).
            '((nonassoc "in") (nonassoc "and"))
            ;; Resolve the "artificial" conflict introduced by the `foo1' rule.
            '((assoc "|") (assoc ";"))
            ;; Fix up associative declaration keywords.
            '((assoc "type" "d-let" "exception" "module" "val" "open"
                     "external" "include" "class" ";;")
              (assoc "and"))
            '((assoc "val" "method" "inherit" "constraint"))
            ;; Declare associativity of remaining sequence separators.
            '((assoc ";")) '((assoc "|")) '((assoc "m-and")))))
      ;; (dolist (pair '()) ;; ("then" . "|") ("|" . "then")
      ;;   (display-warning 'prec2 (format "%s %s %s"
      ;;                                   (car pair)
      ;;                                   (gethash pair bnfprec2)
      ;;                                   (cdr pair))))
      ;; SMIE takes for granted that all non-terminals can match the empty
      ;; string, which can lead to the addition of unnecessary constraints.
      ;; Let's remove the ones that cause cycles without causing conflicts.
      (progn
        ;; This comes from "exp ; exp" and "function branches", where
        ;; SMIE doesn't realize that `branches' has to have a -> before ;.
        (assert (eq '> (gethash (cons "function" ";") bnfprec2)))
        (remhash (cons "function" ";") bnfprec2))
      (smie-prec2->grammar
       (smie-merge-prec2s
        bnfprec2
        (smie-precs->prec2
         ;; Precedence of operators.
         ;; http://caml.inria.fr/pub/docs/manual-ocaml/expr.html
         (reverse
          '((nonassoc "." "#")
            ;; function application, constructor application, assert, lazy
            ;; - -. (prefix)	–
            (right "**…" "lsl" "lsr" "asr")
            (nonassoc "*…" "/…" "%…" "mod" "land" "lor" "lxor")
            (left "+…" "-…")
            (assoc "::")
            (right "@…" "^…")
            (left "=…" "<…" ">…" "|…" "&…" "$…")
            (right "&" "&&")
            (right "or" "||")
            (assoc ",")
            (right "<-" ":=")
            (assoc ";")))))))))

(defun tuareg-smie--search-backward (tokens)
  (let (tok)
    (while (progn
             (setq tok (tuareg-smie--backward-token))
             (if (not (zerop (length tok)))
                 (not (member tok tokens))
               (condition-case err
                   (progn (backward-sexp) t)
                 (scan-error
                  (setq tok (buffer-substring (nth 3 err) (1+ (nth 3 err))))
                  nil)))))
    tok))

(defconst tuareg-smie--type-label-leader
  '("->" ":" "=" ""))
(defconst tuareg-smie--exp-operator-leader
  (delq nil (mapcar (lambda (x) (if (numberp (nth 2 x)) (car x)))
                    tuareg-smie-grammar)))
(defconst tuareg-smie--float-re "[0-9]+\\(?:\\.[0-9]*\\)?\\(?:e[-+]?[0-9]+\\)")

(defun tuareg-smie--forward-token ()
  (forward-comment (point-max))
  (buffer-substring-no-properties
   (point)
   (progn (if (zerop (skip-syntax-forward "."))
              (let ((start (point)))
                (skip-syntax-forward "w_'")
                ;; Watch out for floats!
                (and (memq (char-after) '(?- ?+))
                     (eq (char-before) ?e)
                     (save-excursion
                       (goto-char start)
                       (looking-at tuareg-smie--float-re))
                     (goto-char (match-end 0))))
            ;; The "." char is given symbol property so that "M.x" is
            ;; considered as a single symbol, but in reality, it's part of the
            ;; operator chars, since "+." and friends are operators.
            (while (not (and (zerop (skip-chars-forward "."))
                             (zerop (skip-syntax-forward "."))))))
          (point))))

(defun tuareg-smie--backward-token ()
  (forward-comment (- (point)))
  (buffer-substring-no-properties
   (point)
   (progn (if (and (zerop (skip-chars-backward "."))
                   (zerop (skip-syntax-backward ".")))
              (progn
                (skip-syntax-backward "w_'")
                ;; Watch out for floats!
                (and (memq (char-before) '(?- ?+))
                     (memq (char-after) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0))
                     (save-excursion
                       (forward-char -1) (skip-syntax-backward "w_")
                       (looking-at tuareg-smie--float-re))
                     (>= (match-end 0) (point))
                     (goto-char (match-beginning 0))))
            (cond
             ((memq (char-after) '(?\; ?,)) nil) ; ".;" is not a token.
             ((and (eq (char-after) ?\.)
                   (memq (char-before) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0)))
              (skip-chars-backward "0-9")) ; A float number!
             (t ;; The "." char is given symbol property so that "M.x" is
              ;; considered as a single symbol, but in reality, it's part of
              ;; the operator chars, since "+." and friends are operators.
              (while (not (and (zerop (skip-chars-backward "."))
                               (zerop (skip-syntax-backward "."))))))))
          (point))))

(defun tuareg-smie-forward-token ()
  (let ((tok (tuareg-smie--forward-token)))
    (cond
     ((zerop (length tok))
      (if (not (looking-at "{<\\|\\[|"))
          tok
        (goto-char (match-end 0))
        (match-string 0)))
     ((or (member tok '("let" "=" "->"
                        "module" "class" "open" "type" "with" "and"))
          ;; http://caml.inria.fr/pub/docs/manual-ocaml/expr.html lists
          ;; the tokens whose precedence is based on their prefix.
          (memq (aref tok 0) '(?* ?/ ?% ?+ ?- ?@ ?^ ?= ?< ?> ?| ?& ?$)))
      ;; When indenting, the movement is mainly backward, so it's OK to make
      ;; the forward tokenizer a bit slower.
      (save-excursion (tuareg-smie-backward-token)))
     ((and (member tok '("~" "?"))
           (looking-at "[[:alpha:]_][[:alnum:]'_]*:"))
      (goto-char (match-end 0))
      "label:")
     ((and (looking-at ":\\(?:[^:]\\|\\'\\)")
           (string-match "\\`[[:alpha:]_]" tok)
           (save-excursion
             (tuareg-smie--backward-token) ;Go back.
             (member (tuareg-smie--backward-token)
                     tuareg-smie--type-label-leader)))
      (forward-char 1)
      "label:")
     ((and (equal tok "|") (looking-at "\\]")) (forward-char 1) "|]")
     ((and (equal tok ">") (looking-at "}")) (forward-char 1) ">}")
     ((string-match "\\`[[:alpha:]_].*\\.\\'"  tok)
      (forward-char -1) (substring tok 0 -1))
     (t tok))))

(defconst tuareg-smie--exp-leaders
  ;; (let ((leaders ()))
  ;;   (dolist (cat tuareg-smie-bnf)
  ;;     (dolist (rule (cdr cat))
  ;;       (setq rule (reverse rule))
  ;;       (while (setq rule (cdr (memq 'exp rule)))
  ;;         (push (car rule) leaders))))
  ;;   leaders)
  '("if" "then" "try" "match" "do" "while" "begin" "in" "when"
    "downto" "to" "else"))

(defun tuareg-smie--label-colon-p ()
  (and (not (zerop (skip-chars-backward "[[:alnum:]]_")))
       (or (not (zerop (skip-chars-backward "?~")))
           (save-excursion
             (member (tuareg-smie--backward-token)
                     tuareg-smie--type-label-leader)))))

(defun tuareg-smie--=-disambiguate ()
  "Return which kind of \"=\" we've just found.
Point is not moved and should be right in front of the equality.
Return values can be \"f=\" for field definition, \"d=\" for a normal definition,
\"c=\" for a type equality constraint, and \"=…\" for an equality test."
  (save-excursion
    (let* ((pos (point))
           (telltale '("type" "let" "module" "class" "and" "external"
                       "=" "if" "then" "else" "->" ";"))
           (nearest (tuareg-smie--search-backward telltale)))
      (cond
       ((and (member nearest '("{" ";"))
             (let ((field t))
               (while
                   (let ((x (tuareg-smie--forward-token)))
                     (and (< (point) pos)
                          (cond
                           ((zerop (length x)) (setq field nil))
                           ((memq (char-syntax (aref x 0)) '(?w ?_)))
                           ((member x '("." ";")))
                           (t (setq field nil))))))
               field))
        "f=")
       ((progn
          (while (and (equal nearest "->")
                      (save-excursion
                        (forward-char 2)
                        (equal (tuareg-smie-backward-token) "t->")))
            (setq nearest (tuareg-smie--search-backward telltale)))
          nil))
       ((not (member nearest
                     '("type" "let" "module" "class" "and" "external")))
        "=…")
       ((and (member nearest '("type" "module"))
             (member (tuareg-smie--backward-token) '("with" "and"))) "c=")
       (t "d=")))))

(defun tuareg-smie-backward-token ()
  (let ((tok (tuareg-smie--backward-token)))
    (cond
     ;; Distinguish a let expression from a let declaration.
     ((equal tok "let")
      (save-excursion
        (let ((prev (tuareg-smie--backward-token)))
          (if (or (member prev tuareg-smie--exp-leaders)
                  (if (zerop (length prev))
                      (and (not (bobp))
                           (eq 4 (mod (car (syntax-after (1- (point)))) 256)))
                    (and (eq ?. (char-syntax (aref prev 0)))
                         (not (equal prev ";;")))))
              tok
            "d-let"))))
     ;; Handle "let module" and friends.
     ((member tok '("module" "class" "open"))
      (let ((prev (save-excursion (tuareg-smie--backward-token))))
        (cond
         ((equal prev "let") (concat "l-" tok))
         ((and (member prev '("with" "and")) (equal tok "module")) "w-module")
         (t tok))))
     ;; Distinguish a "type ->" from a "case ->".
     ((equal tok "->")
      (save-excursion
        (let (nearest)
          (while (progn
                   (setq nearest (tuareg-smie--search-backward
                                  '("with" "|" "fun" "type" ":" "of")))
                   (and (equal nearest ":")
                        (tuareg-smie--label-colon-p))))
          (if (member nearest '("with" "|" "fun"))
              tok "t->"))))
     ;; Handle "module type" and mod-constraint's "with/and type".
     ((equal tok "type")
      (save-excursion
        (let ((prev (tuareg-smie--backward-token)))
          (cond ((equal prev "module") "m-type")
                ((member prev '("and" "with")) "w-type")
                (t tok)))))
     ;; Disambiguate mod-constraint's "and" and "with".
     ((member tok '("with" "and"))
      (save-excursion
        (tuareg-smie--forward-token)
        (if (member (tuareg-smie--forward-token) '("type" "module"))
            (concat "m-" tok) tok)))
     ;; Distinguish a defining = from a comparison-=.
     ((equal tok "=")
      (tuareg-smie--=-disambiguate))
     ((zerop (length tok))
      (if (not (and (memq (char-before) '(?\} ?\]))
                    (save-excursion (forward-char -2)
                                    (looking-at ">}\\||\\]"))))
          tok
        (goto-char (match-beginning 0))
        (match-string 0)))
     ((and (equal tok "|") (eq (char-before) ?\[)) (forward-char -1) "[|")
     ((and (equal tok "<") (eq (char-before) ?\{)) (forward-char -1) "{<")
     ;; Some infix operators get a precedence based on their prefix, so we
     ;; collapse them into a canonical representative.
     ;; See http://caml.inria.fr/pub/docs/manual-ocaml/expr.html.
     ((memq (aref tok 0) '(?* ?/ ?% ?+ ?- ?@ ?^ ?= ?< ?> ?| ?& ?$))
      (cond
       ((member tok '("|" "||" "&" "&&" "<-" "->")) tok)
       ((and (eq (aref tok 0) ?*) (> (length tok) 1) (eq (aref tok 1) ?*))
        "**…")
       (t (string (aref tok 0) ?…))))
     ((equal tok ":")
      (let ((pos (point)))
        (if (tuareg-smie--label-colon-p)
            "label:"
          (goto-char pos)
          tok)))
     ((string-match "\\`[[:alpha:]_].*\\.\\'"  tok)
      (forward-char (1- (length tok))) ".")
     (t tok))))

(defun tuareg-smie-rules (kind token)
  ;; FIXME: Handling of "= |", "with |", "function |", and "[ |" is
  ;; problematic.
  (cond
   ;; Special indentation for module fields.
   ((and (eq kind :after) (member token '("." ";"))
         (smie-rule-parent-p "with")
         (tuareg-smie--with-module-fields-rule)))
   ;; Special indentation for monadic >>>, >>|, and >>= operators.
   ((and (eq kind :before) (tuareg-smie--monadic-rule token)))
   ((member token '(";" "|" "," "and" "m-and"))
    (cond
     ((and (eq kind :before) (member token '("|" ";"))
           (smie-rule-parent-p "then")
           ;; We have misparsed the code: TOKEN is not a child of `then' but
           ;; should have closed the "if E1 then E2" instead!
           (tuareg-smie--if-then-hack token)))
     ;; FIXME: smie-rule-separator doesn't behave correctly when the separator
     ;; is right after the parent (on another line).
     ((smie-rule-prev-p "d=" "with" "[" "function")
      (if (and (eq kind :before) (smie-rule-bolp)
               (smie-rule-prev-p "[" "d=" "function"))
          0 tuareg-with-indent))
     (t (smie-rule-separator kind))))
   (t
    (case kind
      (:elem (if (eq token 'basic) tuareg-default-indent))
      (:list-intro (member token '("fun")))
      (:before
       (cond
	((equal token "d=") (smie-rule-parent 2))
	((member token '("fun" "match"))
         (if (and (not (smie-rule-bolp)) (smie-rule-prev-p "d="))
             (smie-rule-parent tuareg-default-indent)))
	((equal token "then") (smie-rule-parent))
	((equal token "if") (if (and (not (smie-rule-bolp))
				     (smie-rule-prev-p "else"))
				(smie-rule-parent)))
	((and (equal token "with") (smie-rule-parent-p "{"))
	 (smie-rule-parent))
        ;; Align the "with" of "module type A = B \n with ..." w.r.t "module".
	((and (equal token "m-with") (smie-rule-parent-p "d="))
         (save-excursion
           (smie-backward-sexp token)
           (goto-char (nth 1 (smie-backward-sexp 'halfsexp)))
           (cons 'column (+ 2 (current-column)))))
        ;; Treat purely syntactic block-constructs as being part of their
        ;; parent, when the opening statement is hanging.
        ((member token '("let" "(" "[" "{" "struct"))
         (when (and (smie-rule-hanging-p)
                    (apply #'smie-rule-prev-p
                           tuareg-smie--exp-operator-leader))
           (if (let ((openers '("{" "(" "{<" "[" "[|")))
                 (or (apply #'smie-rule-prev-p openers)
                     (not (apply #'smie-rule-parent-p openers))))
               (smie-rule-parent)
             ;; In "{ a = (", "{" and "a =" are not part of the same
             ;; syntax rule, so "(" is part of "a =" but not of the
             ;; surrounding "{".
             (save-excursion
               (smie-backward-sexp 'halfsexp)
               (cons 'column (smie-indent-virtual))))))
        ;; Apparently, people like their `| pattern when test -> body' to have
        ;;  the `when' indented deeper than the body.
        ((equal token "when") (smie-rule-parent tuareg-match-when-indent))))
      (:after
       (cond
        ((equal token "d=")
         (and (smie-rule-parent-p "type")
              (not (smie-rule-next-p "["))
              2))
        ((equal token "->")
         (cond
	  ((and (smie-rule-parent-p "with")
		;; Align with "with" but only if it's the only branch (often
		;; the case in try..with), since otherwise subsequent
		;; branches can't be both indented well and aligned.
		(save-excursion
		  (and (not (equal "|" (nth 2 (smie-forward-sexp "|"))))
		       ;; Since we may misparse "if..then.." we need to
		       ;; double check that smie-forward-sexp indeed got us
		       ;; to the right place.
		       (equal (nth 2 (smie-backward-sexp "|")) "with"))))
	   (smie-rule-parent 2))
	  ((smie-rule-parent-p "|") tuareg-match-clause-indent)
	  (t 0)))
        ((equal token ":")
         (cond
          ((smie-rule-parent-p "val" "external") (smie-rule-parent 2))
          ((smie-rule-parent-p "module") (smie-rule-parent))
          (t 2)))
	((equal token "in") tuareg-in-indent) ;;(if (smie-rule-hanging-p)
	((equal token "with")
	 (cond
	  ;; ((smie-rule-next-p "|") 2)
	  ((smie-rule-parent-p "{") nil)
	  (t (+ 2 tuareg-with-indent))))
        ((or (member token '("." "t->" "]"))
             (consp (nth 2 (assoc token tuareg-smie-grammar)))) ;; Closer.
         nil)
        (t tuareg-default-indent)))))))

(defun tuareg-smie--with-module-fields-rule ()
  ;; Indentation of fields after "{ E with Module." where the "Module."
  ;; syntactically only applies to the first field, but has
  ;; semantically a higher position since it applies to all fields.
  (save-excursion
    (forward-char 1)
    (let ((parent-data (smie-backward-sexp 'halfsexp)))
      (when (looking-at "\\(?:\\sw\\|\\s_\\)+\\.[ \t]*$")
        (smie-backward-sexp 'halfsexp)
        (cons 'column (current-column))))))

(defun tuareg-smie--monadic-rule (token)
  ;; When trying to indent a >>=, try to look back to find any earlier
  ;; >>= in a sequence of "monadic steps".
  (or (and (equal token ">…") (looking-at ">>[>=|]")
           (save-excursion
             (tuareg-smie--forward-token)
             (let ((indent nil))
               (while
                   (let ((parent-data (smie-backward-sexp 'halfsexp)))
                     (cond
                      ((car parent-data) (member (nth 2 parent-data) '("->")))
                      ((member (nth 2 parent-data) '(";" "d=")) nil)
                      ((equal (nth 2 parent-data) "fun")
                       (let ((pos (point)))
                         (if (member (tuareg-smie--backward-token)
                                     '(">>|" ">>=" ">>>"))
                             (progn
                               (setq indent (cons 'column
                                                  (smie-indent-virtual)))
                               nil)
                           t))))))
               indent)))
      ;; In "foo >>= fun x => bar" indent `bar' relative to `foo'.
      (and (equal token "fun") (not (smie-rule-bolp))
           (save-excursion
             (let ((prev (tuareg-smie-backward-token)))
               ;; FIXME: Should we use the same loop as above?
               (and (equal prev ">…") (looking-at ">>[>=]")
                    (progn (smie-backward-sexp prev)
                           (cons 'column (current-column)))))))))

(defun tuareg-smie--if-then-hack (token)
  ;; Getting SMIE's parser to properly parse "if E1 then E2" is difficult, so
  ;; instead we live with a confused parser and try to work around the mess
  ;; here, although it clearly won't help other uses of the parser
  ;; (e.g. navigation).
  (save-excursion
    (let (pd)
      (while (equal (nth 2 (setq pd (smie-backward-sexp token))) "then")
        (let ((pdi (smie-backward-sexp 'halfsexp)))
          (assert (equal (nth 2 pdi) "if"))))
      (cond
       ((equal (nth 2 pd) token)
        (goto-char (nth 1 pd))
        (cons 'column (smie-indent-virtual)))
       (t (cons 'column (current-column)))))))

(defun tuareg-smie--inside-string ()
  (when (nth 3 (syntax-ppss))
    (goto-char (1+ (nth 8 (syntax-ppss))))
    (current-column)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              The major mode

(defun tuareg--common-mode-setup ()
  (setq local-abbrev-table tuareg-mode-abbrev-table)
  (set (make-local-variable 'syntax-propertize-function)
       #'tuareg-syntax-propertize)
  (set (make-local-variable 'parse-sexp-ignore-comments)
       ;; Tuareg used to set this to nil (for an unknown reason) but SMIE needs
       ;; it to be set to t.
       tuareg-use-smie)
  (if (and tuareg-smie-grammar tuareg-use-smie)
      (progn
        (smie-setup tuareg-smie-grammar #'tuareg-smie-rules
                    :forward-token #'tuareg-smie-forward-token
                    :backward-token #'tuareg-smie-backward-token)
        (add-hook 'smie-indent-functions #'tuareg-smie--inside-string nil t))
    (set (make-local-variable 'indent-line-function) #'tuareg-indent-command))
  (tuareg-install-font-lock)
  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)

  (add-hook 'completion-at-point-functions #'tuareg-completion-at-point nil t)

  (when (fboundp 'electric-indent-mode)
    (add-hook 'electric-indent-functions
              #'tuareg--electric-indent-predicate nil t))
  (when (boundp 'post-self-insert-hook)
    (add-hook 'post-self-insert-hook #'tuareg--electric-close-vector nil t)))

;;;###autoload(add-to-list 'auto-mode-alist '("\\.ml[iylp]?\\'" . tuareg-mode))
;;;###autoload(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".annot"))
;;;###autoload  (add-to-list 'completion-ignored-extensions ext))

(defalias 'tuareg--prog-mode
  (if (fboundp 'prog-mode) #'prog-mode #'fundamental-mode))
(defvar compilation-first-column)
(defvar compilation-error-screen-columns)

;;;###autoload
(define-derived-mode tuareg-mode tuareg--prog-mode "Tuareg"
  "Major mode for editing OCaml code.

Dedicated to Emacs and XEmacs, version 21 and higher.  Provides
automatic indentation and compilation interface. Performs font/color
highlighting using Font-Lock.  It is designed for OCaml but handles
Caml Light as well.

The Font-Lock minor-mode is used according to your customization
options.

You have better byte-compile tuareg.el.

For customization purposes, you should use `tuareg-mode-hook'
\(run for every file) or `tuareg-load-hook' (run once) and not patch
the mode itself. You should add to your configuration file something like:
  (add-hook 'tuareg-mode-hook
            (lambda ()
               ... ; your customization code
            ))
For example you can change the indentation of some keywords, the
`electric' flags, Font-Lock colors... Every customizable variable is
documented, use `C-h-v' or look at the mode's source code.

`dot-emacs.el' is a sample customization file for standard changes.
You can append it to your `.emacs' or use it as a tutorial.

`M-x ocamldebug' FILE starts the OCaml debugger ocamldebug on the executable
FILE, with input and output in an Emacs buffer named *ocamldebug-FILE*.

A Tuareg Interactive Mode to evaluate expressions in a toplevel is included.
Type `M-x tuareg-run-ocaml' or see special-keys below.

For the best indentation experience, some elementary rules must be followed.
  - Because the `function' keyword has a special indentation (to handle
    case matches) use the `fun' keyword when no case match is performed.
  - In OCaml, `;;' is no longer necessary for correct indentation,
    except before top level phrases not introduced by `type', `val', `let'
    etc. (i.e., phrases used for their side-effects or to be executed
    in a top level.)
  - Long sequences of `and's may slow down indentation slightly, since
    some computations (few) require to go back to the beginning of the
    sequence. Some very long nested blocks may also lead to slow
    processing of `end's, `else's, `done's...
  - Multiline strings are handled properly, but you may prefer string
    concatenation `^' to break long strings (the C-j keystroke can help).
  - Comment indentation is often a matter of taste and context, yet
    sophisticated heuristics provide reasonable indentation in most cases.
    When inserting a comment right before the code it refers to, it is
    generally expected that this comment will be aligned with the folowing
    code; to enforce this, leave a blank line before the comment.

Known bugs:
  - When writing a line with mixed code and comments, avoid putting
    comments at the beginning or middle of the text. More precisely,
    writing comments immediately after `=' or parentheses then writing
    some more code on the line leads to indentation errors. You may write
    `let x (* blah *) = blah' but should avoid `let x = (* blah *) blah'.

Short cuts for the Tuareg mode:
\\{tuareg-mode-map}

Short cuts for interactions with the toplevel:
\\{tuareg-interactive-mode-map}"

  ;; Initialize the Tuareg menu
  (tuareg-build-menu)

  ;; Initialize indentation regexps
  (tuareg-make-indentation-regexps)

  (set (make-local-variable 'paragraph-start)
       (concat "^[ \t]*$\\|\\*)$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-start-skip) "(\\*+[ \t]*")
  (set (make-local-variable 'comment-column) 40)              ;FIXME: Why?
  (set (make-local-variable 'comment-multi-line) t)           ;FIXME: Why?
  ;; `ocamlc' counts columns from 0, contrary to other tools which start at 1.
  (set (make-local-variable 'compilation-first-column) 0)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (tuareg--common-mode-setup)
  (unless tuareg-use-syntax-ppss
    (add-hook 'before-change-functions 'tuareg-before-change-function nil t))
  (unless (fboundp 'comment-normalize-vars)
    ;; Emacs-21's newcomment.el provides this functionality by default.
    (set (make-local-variable 'normal-auto-fill-function)
         #'tuareg-auto-fill-function))

  (set (make-local-variable 'imenu-create-index-function)
       #'tuareg-imenu-create-index)

  (when (and tuareg-use-abbrev-mode
             (not (and (boundp 'electric-indent-mode) electric-indent-mode)))
    (abbrev-mode 1))
  (message nil))

(defun tuareg-install-font-lock ()
  (setq
   tuareg-font-lock-keywords
   `(,@(if (tuareg-editing-ls3)
           `((,(concat "\\<\\(let[ \t\n]+"
                       (regexp-opt '("clock" "node" "static")) "\\|"
                       (regexp-opt '("present" "automaton" "where" "match"
                                     "with" "do" "done" "unless" "until"
                                     "reset" "every"))
                       "\\)\\>")
              0 tuareg-font-lock-governing-face nil nil)))
     (,(concat "\\<\\("
               (regexp-opt '("external" "open" "include" "sig" "struct"
                             "module" "functor" "val" "type" "method"
                             "virtual" "constraint" "class" "in" "inherit"
                             "initializer" "let" "rec" "object" "and" "begin"
                             "end"))
               "\\|with[ \t\n]+\\(type\\|module\\)\\)\\>")
      0 tuareg-font-lock-governing-face nil nil)
     ,@(and tuareg-support-metaocaml
            '(("\\.<\\|>\\.\\|\\.~\\|\\.!"
               0 tuareg-font-lock-multistage-face nil nil)))
     ("\\<\\(false\\|true\\)\\>" 0 font-lock-constant-face nil nil)
     (,(regexp-opt '("as" "do" "of" "done" "downto" "else" "for" "if"
		     "mutable" "new" "parser" "private"
		     "then" "to" "try" "when" "while" "match" "with"
		     "lazy" "exception" "raise" "failwith" "failwithf"
		     "exit" "assert" "fun" "function") 'words)
      0 font-lock-keyword-face nil nil)
     ,@(if (tuareg-editing-ls3)
           `(("\\<\\(merge\\|when\\|emit\\|period\\)\\>"
              0 font-lock-keyword-face nil nil)))
     (,(concat
         "[][;,()|{}]\\|[@^!:*=<>&/%+~?#---]\\.?\\|\\.\\.\\.*\\|"
         (if (tuareg-editing-ls3)
             (regexp-opt '("asr" "asl" "lsr" "lsl" "or" "lor" "and" "land"
                           "lxor" "not" "lnot" "mod" "of" "ref"
                           "fby" "pre" "last" "at") 'words)
           (regexp-opt '("asr" "asl" "lsr" "lsl" "or" "lor" "and" "land"
                         "lxor" "not" "lnot" "mod" "of" "ref") 'words)))
       0 tuareg-font-lock-operator-face nil nil)
     (,(concat
        "\\<\\(\\(method\\([ \t\n]+\\(private\\|virtual\\)\\)*\\)"
        "\\|val\\([ \t\n]+mutable\\)?"
        "\\|external\\|and\\|class"
        (if (tuareg-editing-ls3)
            "\\|let\\([ \t\n]+\\(?:rec\\|clock\\|node\\|static\\)\\)?"
          "\\|let\\([ \t\n]+rec\\)?")
        "\\)\\>[ \t\n]*\\([_[:lower:]]\\(\\w\\|[._]\\)*\\)\\>"
        "[ \t\n]*\\(\\(\\w\\|[()_?~.'*:--->]\\)+"
        "\\|=[ \t\n]*fun\\(ction\\)?\\>\\)")
      7 font-lock-function-name-face keep nil)
     ;; FIXME: Isn't this redundant with the previous one?
     ("\\<method\\([ \t\n]+\\(private\\|virtual\\)\\)?\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)"
      3 font-lock-function-name-face keep nil)
     ("\\<\\(fun\\(ction\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_ \t()*,]\\)+\\)"
      3 font-lock-variable-name-face keep nil)
     (,(concat
        "\\<\\("
        (if (tuareg-editing-ls3) "reset\\|do\\|")
        "val\\([ \t\n]+mutable\\)?\\|external\\|and\\|class"
        "\\|let\\([ \t\n]+rec\\)?"
        "\\)\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)")
      4 font-lock-variable-name-face keep nil)
     (,(concat
        "\\<\\("
        (if (tuareg-editing-ls3) "reset\\|do\\|")
        "val\\([ \t\n]+mutable\\)?\\|external\\|method\\|and\\|class"
        "\\|let\\([ \t\n]+"
        (if (tuareg-editing-ls3) "\\(?:rec\\|clock\\|node\\|static\\)" "rec")
        "\\)?\\)\\>[ \t\n]*\\(\\(\\w\\|[_,?~.]\\)*\\)\\>"
        "\\(\\(\\w\\|[->_ \t,?~.]\\|(\\(\\w\\|[--->_ \t,?~.=]\\)*)\\)*\\)")
      6 font-lock-variable-name-face keep nil)
     (,(concat
        "\\<\\(open\\|\\(class\\([ \t\n]+type\\)?\\)\\([ \t\n]+virtual\\)?"
        "\\|inherit\\|include\\|module\\([ \t\n]+\\(type\\|rec\\)\\)?"
        "\\|type\\)\\>[ \t\n]*"
        "\\(['~?]*\\([_--->.* \t]\\|\\w\\|(['~?]*\\([_--->.,* \t]\\|\\w\\)*)\\)*\\)")
      7 font-lock-type-face keep nil)
     (,(concat
        "\\(?:"
        (if (tuareg-editing-ls3)
            "\\<val\\>[ \t\n]*\\w*[ \t\n]*:\\|")
        "[^:>=]\\):[ \t\n]*"
        "\\(['~?]*\\([_--->.* \t]\\|\\w\\|(['~?]*\\([_--->.,* \t]\\|\\w\\)*)\\)*\\)")
      1 font-lock-type-face keep nil)
     ("\\<\\([A-Z]\\w*\\>\\)[ \t]*\\." 1 font-lock-type-face keep nil)
     ("\\<\\([?~]?[_[:alpha:]]\\w*\\)[ \t\n]*:[^:>=]"
      1 font-lock-variable-name-face keep nil)
     ("\\<exception\\>[ \t\n]*\\(\\<[_[:alpha:]]\\w*\\>\\)"
      1 font-lock-variable-name-face keep nil)
     ("^#\\w+\\>" 0 font-lock-preprocessor-face t nil)
     ,@(and tuareg-font-lock-symbols
            (tuareg-font-lock-symbols-keywords))))
  (setq font-lock-defaults
        `(tuareg-font-lock-keywords
          ,(not tuareg-use-syntax-ppss) nil
          ,tuareg-font-lock-syntax nil
          ,@(unless (fboundp 'tuareg-syntax-propertize)
              '((font-lock-syntactic-keywords
                 . tuareg-font-lock-syntactic-keywords)
                (parse-sexp-lookup-properties . t)))
          (font-lock-syntactic-face-function
           . tuareg-font-lock-syntactic-face-function)
          ,@(unless tuareg-use-syntax-ppss
              '((font-lock-fontify-region-function
                 . tuareg-fontify-region)))))
  (when (and (boundp 'font-lock-fontify-region-function)
             (not tuareg-use-syntax-ppss))
    (set (make-local-variable 'font-lock-fontify-region-function)
         'tuareg-fontify-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Error processing

(require 'compile)

;; In some versions of Emacs, the regexps in
;; compilation-error-regexp-alist do not match the error messages when
;; the language is not English.  Hence we add a regexp.
;; FIXME: We should report those cases to bug-gnu-emacs@gnu.org.

(defconst tuareg-error-regexp
  ;; Errors can take forms like:
  ;;   "File "main.ml", line 1154, characters 30-48:\nError: ..."
  ;;   "Raised at file "pervasives.ml", line 22, characters 22-33"
  ;;   "File "main.ml", line 1018, characters 2-2632:\nWarning 8: ..."
  ;; as well as localized variants depending on locale.
  (concat "^\\(Called from \\)?[[:alpha:]][ [:alpha:]]*[[:alpha:]] "
          "\"\\([^\"\n]+\\)\", "                              ;File name.
          "[[:alpha:]]+ \\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?, " ;Lines.
          "[[:alpha:]]+ \\([0-9]+\\)-\\([0-9]+\\)"            ;Columns.
          "\\(?::\\(\nWarning\\)?\\|[-,:]\\|$\\)")            ;Warning/error.
  "Regular expression matching the error messages produced by ocamlc.")

(when (boundp 'compilation-error-regexp-alist)
  (or (assoc tuareg-error-regexp
             compilation-error-regexp-alist)
      (setq compilation-error-regexp-alist
            (cons (if (fboundp 'compilation-fake-loc)
                      (list tuareg-error-regexp
                            2 '(3 . 4) '(5 . 6) '(7 . 1))
                    (list tuareg-error-regexp 2 3))
                  compilation-error-regexp-alist))))

;; A regexp to extract the range info.

;; (defconst tuareg-error-chars-regexp
;;   ".*, .*, [^\0-@]+ \\([0-9]+\\)-\\([0-9]+\\):"
;;   "Regexp matching the char numbers in an error message produced by ocamlc.")

;; Wrapper around next-error.

;; itz 04-21-96 instead of defining a new function, use defadvice
;; that way we get our effect even when we do \C-x` in compilation buffer

;; smclaughlin 07-19-11 defadvice is to be avoided.  It makes debugging
;; much more difficult.  If you really want this behavior, write your
;; own next-error-function.  In particular, it breaks when omake is
;; used.

;; (defadvice next-error (after tuareg-next-error activate)
;;  "Read the extra positional information provided by the OCaml compiler.

;; Puts the point and the mark exactly around the erroneous program
;; fragment. The erroneous fragment is also temporarily highlighted if
;; possible."
;;  (when (eq major-mode 'tuareg-mode)
;;    (let ((beg nil) (end nil))
;;      (with-current-buffer compilation-last-buffer
;;        (save-excursion
;;          (goto-char (window-point (get-buffer-window (current-buffer) t)))
;;          (when (looking-at tuareg-error-chars-regexp)
;;            (setq beg (string-to-number (tuareg-match-string 1))
;;                  end (string-to-number (tuareg-match-string 2))))))
;;      (beginning-of-line)
;;      (when beg
;;        (setq beg (+ (point) beg) end (+ (point) end))
;;        (goto-char beg) (push-mark end t t)))))

(defvar tuareg-interactive-error-regexp
  (concat "\\(\\("
          "Toplevel input:"
          "\\|Entr.e interactive:"
          "\\|Characters [0-9-]*:"
          "\\|The global value [^ ]* is referenced before being defined."
          "\\|La valeur globale [^ ]* est utilis.e avant d'.tre d.finie."
          "\\|Reference to undefined global"
          "\\|The C primitive \"[^\"]*\" is not available."
          "\\|La primitive C \"[^\"]*\" est inconnue."
          "\\|Cannot find \\(the compiled interface \\)?file"
          "\\|L'interface compil.e [^ ]* est introuvable."
          "\\|Le fichier [^ ]* est introuvable."
          "\\|Exception non rattrap.e:"
          "\\|Uncaught exception:"
          "\\)[^#]*\\)" )
  "Regular expression matching the error messages produced by OCaml.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Indentation stuff

(eval-and-compile
  (defconst tuareg-no-more-code-this-line-regexp "[ \t]*\\((\\*\\|$\\)"
    "Regexp matching lines which have no more code:
 blanks + (maybe) comment start."))

(defmacro tuareg-no-code-after (rex)
  `(eval-when-compile (concat ,rex tuareg-no-more-code-this-line-regexp)))

(defconst tuareg-no-code-this-line-regexp
  (concat "^" tuareg-no-more-code-this-line-regexp))

(defun tuareg-ro (&rest words) (concat "\\<" (regexp-opt words t) "\\>"))

(defconst tuareg-extra-unindent-regexp
  (concat "\\(" (tuareg-ro "with" "fun" "function")
          "\\|\\[" tuareg-no-more-code-this-line-regexp "\\)")
  "Regexp for keywords needing extra indentation to compensate for case matches.")

(defconst tuareg-ls3-extras (concat "\\|" (tuareg-ro "automaton" "present")))

(defconst tuareg-extra-unindent-regexp-ls3
  (concat tuareg-extra-unindent-regexp tuareg-ls3-extras)
  "Regexp for keywords needing extra indentation to compensate for case matches.")

(defun tuareg-give-extra-unindent-regexp ()
  (if (tuareg-editing-ls3)
      tuareg-extra-unindent-regexp-ls3
    tuareg-extra-unindent-regexp))

(defconst tuareg-keyword-regexp
  (concat (tuareg-ro "object" "initializer" "and" "constraint" "class"
                     "match" "module" "method" "mutable" "sig" "struct" "begin"
                     "else" "exception" "external" "to" "then" "try" "type"
                     "virtual" "val" "while" "when" "with" "if" "in" "inherit"
                     "for" "fun" "functor" "function" "let" "do" "downto"
                     "of")
          "\\|->\\|[;,|]")
  "Regexp for all recognized keywords.")

(defconst tuareg-keyword-regexp-ls3
  (concat tuareg-keyword-regexp "\\|"
          (tuareg-ro "where" "automaton" "present" "fby" "pre" "last" "merge"
                     "when" "reset" "every" "emit" "until" "unless" "period"
                     "at"))
  "Regexp for all recognized keywords.
For synchronous programming.")

(defun tuareg-give-keyword-regexp ()
  (if (tuareg-editing-ls3)
      tuareg-keyword-regexp-ls3
    tuareg-keyword-regexp))

(defconst tuareg-match-pipe-kwop-regexp
  (concat (tuareg-ro "and" "function" "type" "with")
          "\\|[[({=]\\||[^!]")
  "Regexp for keywords supporting case match.")

(defconst tuareg-match-pipe-kwop-regexp-ls3
  (concat tuareg-match-pipe-kwop-regexp tuareg-ls3-extras)
  "Regexp for keywords supporting case match.
For synchronous programming.")

(defun tuareg-give-match-pipe-kwop-regexp ()
  (if (tuareg-editing-ls3)
      tuareg-match-pipe-kwop-regexp-ls3
    tuareg-match-pipe-kwop-regexp))

(defconst tuareg-operator-regexp "[---+*/=<>@^&|]\\|:>\\|::\\|\\<\\(or\\|l\\(and\\|x?or\\|s[lr]\\)\\|as[lr]\\|mod\\)\\>"
  "Regexp for all operators.")

(defconst tuareg-matching-keyword-regexp
  (tuareg-ro "and" "do" "done" "then" "else" "end" "in" "downto")
  "Regexp matching OCaml keywords which act as end block delimiters.")

(defconst tuareg-extra-ls3-keyword-regexp
  (tuareg-ro "where" "unless" "until" "every")
  "Additional Lucid Synchrone keywords.")

(defconst tuareg-matching-keyword-regexp-ls3
  (concat tuareg-matching-keyword-regexp "\\|" tuareg-extra-ls3-keyword-regexp)
  "Regexp matching OCaml keywords which act as end block delimiters
For synchronous programming.")

(defun tuareg-give-matching-keyword-regexp ()
  (let ((rxp (if (tuareg-editing-ls3)
                 tuareg-matching-keyword-regexp-ls3
               tuareg-matching-keyword-regexp)))
    (if tuareg-support-metaocaml
        (concat rxp "\\|>\\.")
      rxp)))

(defconst tuareg-matching-kwop-regexp
  (concat tuareg-matching-keyword-regexp
          "\\|\\<with\\>\\|[|>]?\\]\\|>?}\\|[|)]\\|;;")
  "Regexp matching OCaml keywords or operators which act as end block
delimiters.")

(defconst tuareg-matching-kwop-regexp-ls3
  (concat tuareg-matching-kwop-regexp "\\|" tuareg-extra-ls3-keyword-regexp)
  "Regexp matching OCaml keywords or operators which act as end block
delimiters.  For synchronous programming.")

(defun tuareg-give-matching-kwop-regexp ()
  (if (tuareg-editing-ls3)
      tuareg-matching-kwop-regexp-ls3
    tuareg-matching-kwop-regexp))

(defconst tuareg-block-regexp
  (concat (tuareg-ro "for" "while" "do" "if" "begin" "sig" "struct" "object")
          "\\|[][(){}]\\|\\*)"))

(defconst tuareg-find-kwop-regexp
  (concat tuareg-matching-keyword-regexp "\\|" tuareg-block-regexp))

(defconst tuareg-find-kwop-regexp-ls3
  (concat tuareg-find-kwop-regexp "\\|"
          (tuareg-ro "where" "automaton" "present" "match")))

(defun tuareg-give-find-kwop-regexp ()
  (if (tuareg-editing-ls3)
      tuareg-find-kwop-regexp-ls3
    tuareg-find-kwop-regexp))

(defconst tuareg-governing-phrase-regexp
  (tuareg-ro "val" "type" "method" "module" "constraint" "class" "inherit"
             "initializer" "external" "exception" "open" "let" "object"
             "include")
  "Regexp matching tuareg phrase delimitors.")

(defconst tuareg-keyword-alist
  '(("module" . tuareg-default-indent)
    ("class" . tuareg-class-indent)
    ("sig" . tuareg-sig-struct-indent)
    ("struct" . tuareg-sig-struct-indent)
    ("method" . tuareg-method-indent)
    ("object" . tuareg-begin-indent)
    ("begin" . tuareg-begin-indent)
    (".<" . tuareg-begin-indent)
    ("for" . tuareg-for-while-indent)
    ("while" . tuareg-for-while-indent)
    ("do" . tuareg-do-indent)
    ("val" . tuareg-val-indent)
    ("fun" . tuareg-fun-indent)
    ("if" . tuareg-if-then-else-indent)
    ("then" . tuareg-if-then-else-indent)
    ("else" . tuareg-if-then-else-indent)
    ("let" . tuareg-let-indent)
    ("match" . tuareg-match-indent)
    ("try" . tuareg-try-indent)

    ;; Case match keywords
    ("function" . tuareg-function-indent)
    ("with" . tuareg-with-indent)
    ("automaton" . tuareg-with-indent)
    ("present" . tuareg-with-indent)
    ("type" . tuareg-type-indent) ; sometimes, `type' acts like a case match

    ;; Assume default indentation for other keywords and operators
    )
  "Association list of indentation values based on governing keywords.")

(defconst tuareg-leading-kwop-alist
  '(("|" . tuareg-find-pipe-match)
    ("}" . tuareg-find-match)
    (">}" . tuareg-find-match)
    (">." . tuareg-find-match)
    (")" . tuareg-find-match)
    ("]" . tuareg-find-match)
    ("|]" . tuareg-find-match)
    (">]" . tuareg-find-match)
    ("end" . tuareg-find-match)
    ("done" . tuareg-find-done-match)
    ("unless" . tuareg-find-done-match)
    ("until" . tuareg-find-done-match)
    ("every" . tuareg-find-done-match)
    ("in" . tuareg-find-in-match)
    ("where" . tuareg-find-in-match)
    ("with" . tuareg-find-with-match)
    ("else" . tuareg-find-else-match)
    ("then" . tuareg-find-then-match)
    ("do" . tuareg-find-do-match)
    ("to" . tuareg-find-match)
    ("downto" . tuareg-find-match)
    ("and" . tuareg-find-and-match))
  "Association list used in Tuareg mode for skipping back over nested blocks.")

(defun tuareg-find-leading-kwop-match (kwop)
  (funcall (cdr (assoc kwop tuareg-leading-kwop-alist))))

(defconst tuareg-binding-regexp "\\(\\<and\\>\\|(*\\<let\\>\\)")

(defun tuareg-assoc-indent (kwop &optional look-for-let-or-and)
  "Return relative indentation of the keyword given in argument."
  (let ((ind (or (symbol-value (cdr (assoc kwop tuareg-keyword-alist)))
                 tuareg-default-indent))
        (looking-let-or-and (and look-for-let-or-and
                                 (looking-at tuareg-binding-regexp))))
    (if (string-match (tuareg-give-extra-unindent-regexp) kwop)
        (- (if (and tuareg-let-always-indent
                    looking-let-or-and (< ind tuareg-let-indent))
               tuareg-let-indent ind)
           tuareg-pipe-extra-unindent)
      ind)))

(defun tuareg-in-monadic-op-p (&optional pos)
  (unless pos (setq pos (point)))
  (and (char-equal ?> (char-before pos))
       (char-equal ?> (char-before (1- pos)))))

(defconst tuareg-meaningful-word-regexp
  "[^ \t\n_[:alnum:]]\\|\\<\\(\\w\\|_\\)+\\>\\|\\*)")
(defun tuareg-find-meaningful-word ()
  "Look back for a word, skipping comments and blanks.
Returns the actual text of the word, if found."
  (let ((found nil) (kwop nil) (pt (point)))
    (while (and (not found)
                (re-search-backward tuareg-meaningful-word-regexp
                                    (point-min) t))
      (setq kwop (tuareg-match-string 0))
      (cond ((and (or (string= kwop "|") (string= kwop "=") (string= kwop ">"))
                  (tuareg-in-monadic-op-p))
             (backward-char 2)
             (setq kwop (concat ">>" kwop)))
            ((and (string= kwop ">") (char-equal ?- (char-before)))
             (backward-char)
             (setq kwop "->")))
      (when (= pt (point))
        (error "tuareg-find-meaningful-word: inf loop at %d, kwop=%s" pt kwop))
      (setq pt (point))
      (if kwop
          (if (tuareg-in-comment-p)
              (tuareg-beginning-of-literal-or-comment-fast)
            (setq found t))
        (setq found t)))
    (if found kwop (goto-char (point-min)) nil)))

(defun tuareg-make-find-kwop-regexp (kwop-regexp)
  "Make a custom indentation regexp."
  (concat (tuareg-give-find-kwop-regexp) "\\|" kwop-regexp))

;; Dynamic regexps (for language changes, see `tuareg-editing-ls3')
(defvar tuareg-find-comma-match-regexp nil)
(defvar tuareg-find-with-match-regexp nil)
(defvar tuareg-find-in-match-regexp nil)
(defvar tuareg-find-else-match-regexp nil)
(defvar tuareg-find-do-match-regexp nil)
(defvar tuareg-find-=-match-regexp nil)
(defvar tuareg-find-pipe-match-regexp nil)
(defvar tuareg-find-arrow-match-regexp nil)
(defvar tuareg-find-semicolon-match-regexp nil)
(defvar tuareg-find-phrase-indentation-regexp nil)
(defvar tuareg-find-phrase-indentation-break-regexp nil)
(defvar tuareg-find-phrase-indentation-class-regexp nil)
(defvar tuareg-compute-argument-indent-regexp nil)
(defvar tuareg-compute-normal-indent-regexp nil)
(defvar tuareg-find-module-regexp nil)
(defvar tuareg-find-pipe-bang-match-regexp nil)
(defvar tuareg-find-monadic-match-regexp nil)

;; Static regexps
(defconst tuareg-find-and-match-regexp
  (concat (tuareg-ro "do" "done" "else" "end" "in" "then" "downto"
                     "for" "while" "do" "if" "begin" "sig" "struct" "class"
                     "exception" "let" "in" "type" "val" "module")
          "\\|[][(){}]\\|\\*)"))
(defconst tuareg-find-phrase-beginning-regexp
  (concat (tuareg-ro "end" "type" "module" "sig" "struct" "class"
                     "exception" "open" "let")
          "\\|^#[ \t]*[a-z][_a-z]*\\>\\|;;"))
(defconst tuareg-find-phrase-beginning-and-regexp
  (concat "\\<\\(and\\)\\>\\|" tuareg-find-phrase-beginning-regexp))
(defconst tuareg-back-to-paren-or-indentation-regexp
  "[][(){}]\\|\\.<\\|>\\.\\|\\*)\\|^[ \t]*\\(.\\|\n\\)")

;; Specific regexps for module/class detection
(defconst tuareg-inside-module-or-class-opening
  (tuareg-ro "struct" "sig" "object"))
(defconst tuareg-inside-module-or-class-opening-full
  (concat tuareg-inside-module-or-class-opening "\\|"
          (tuareg-ro "module" "class")))
(defconst tuareg-inside-module-or-class-regexp
  (concat (tuareg-give-matching-keyword-regexp) "\\|"
          tuareg-inside-module-or-class-opening))

(defun tuareg-make-indentation-regexps ()
  "Initialisation of specific indentation regexp.
Gathered here for memoization and dynamic reconfiguration purposes."
  (setq
   tuareg-find-comma-match-regexp
    (tuareg-make-find-kwop-regexp
     (concat (tuareg-ro "and" "match" "begin" "else" "exception" "then" "try"
                        "with" "or" "fun" "function" "let" "do")
             "\\|->\\|[[{(]"))
   tuareg-find-with-match-regexp
    (tuareg-make-find-kwop-regexp
     (concat (tuareg-ro "match" "try" "module" "begin" "with" "type")
             "\\|[[{(]"))
   tuareg-find-in-match-regexp
;;    (tuareg-make-find-kwop-regexp (tuareg-ro "let" "open"))
    (tuareg-make-find-kwop-regexp (tuareg-ro "let"))
   tuareg-find-else-match-regexp
    (tuareg-make-find-kwop-regexp ";")
   tuareg-find-do-match-regexp
    (tuareg-make-find-kwop-regexp "->")
   tuareg-find-=-match-regexp
    (tuareg-make-find-kwop-regexp
     (concat (tuareg-ro "val" "let" "method" "module" "type" "class" "when"
                        "if" "in" "do")
             "\\|="))
   tuareg-find-pipe-match-regexp
    (tuareg-make-find-kwop-regexp (tuareg-give-match-pipe-kwop-regexp))
   tuareg-find-arrow-match-regexp
    (tuareg-make-find-kwop-regexp
     (concat (tuareg-ro "external" "type" "val" "method" "let" "with" "fun"
                        "function" "functor" "class")
             "\\|[|;]"))
   tuareg-find-semicolon-match-regexp
    (tuareg-make-find-kwop-regexp
     (concat ";" tuareg-no-more-code-this-line-regexp "\\|->\\|"
             (tuareg-ro "let" "method" "with" "try" "initializer")))
   tuareg-find-phrase-indentation-regexp
    (tuareg-make-find-kwop-regexp
     (concat tuareg-governing-phrase-regexp "\\|" (tuareg-ro "and" "every")))
   tuareg-find-phrase-indentation-break-regexp
    (concat tuareg-find-phrase-indentation-regexp "\\|;;")
   tuareg-find-phrase-indentation-class-regexp
    (concat (tuareg-give-matching-keyword-regexp) "\\|\\<class\\>")
   tuareg-compute-argument-indent-regexp
    (tuareg-make-find-kwop-regexp
     (concat (tuareg-give-keyword-regexp) "\\|="))
   tuareg-compute-normal-indent-regexp
    (concat tuareg-compute-argument-indent-regexp "\\|^.[ \t]*")
   tuareg-find-module-regexp
    (tuareg-make-find-kwop-regexp "\\<module\\>")
   tuareg-find-pipe-bang-match-regexp
    (concat tuareg-find-comma-match-regexp "\\|=")
   tuareg-find-monadic-match-regexp
    (concat tuareg-block-regexp "\\|\\([;=]\\)\\|\\(->\\)\\|"
            (tuareg-ro "val" "let" "method" "module" "type" "class" "when"
                       "if" "in" "do" "done" "end"))))

(defun tuareg-strip-trailing-whitespace (string)
  (if (string-match "[ \t]*\\'" string)
      (substring string 0 (match-beginning 0))
    string))

(defun tuareg-find-kwop-pos (kr do-not-skip-regexp may-terminate-early)
  "Look back for a keyword or operator matching KR (short for kwop regexp).
Skips blocks etc...

Ignore occurences inside literals and comments.
If found, return the actual text of the keyword or operator."
  (let ((found nil)
        (kwop nil) pos
        (kwop-regexp (if tuareg-support-metaocaml
                         (concat kr "\\|\\.<\\|>\\.")
                       kr)))
    (while (and (not found)
                (setq pos (re-search-backward kwop-regexp (point-min) t))
                (setq kwop (tuareg-strip-trailing-whitespace
                            ;; for trailing blanks after a semicolon
                            (tuareg-match-string 0))))
      (cond
       ((tuareg-in-literal-or-comment-p)
        (tuareg-beginning-of-literal-or-comment-fast))
       ((looking-at "[]})]")
        (tuareg-backward-up-list))
       ((tuareg-at-phrase-break-p)
        (setq found t))
       ((and do-not-skip-regexp (looking-at do-not-skip-regexp))
        (if (and (string= kwop "|") (char-equal ?| (preceding-char)))
            (backward-char)
          (setq found t)))
       ((looking-at (tuareg-give-matching-keyword-regexp))
        (let ((mkwop (tuareg-find-leading-kwop-match (tuareg-match-string 0))))
          (when (and may-terminate-early (string-match kwop-regexp mkwop))
            (setq found t))))
       (t
        (setq found t))))
    (if found (list kwop pos) (goto-char (point-min)) nil)))

(defun tuareg-find-kwop (kr &optional do-not-skip-regexp)
  (car (tuareg-find-kwop-pos kr do-not-skip-regexp nil)))

(defun tuareg-find-match ()
  (let ((kwop (tuareg-find-kwop (tuareg-give-find-kwop-regexp))))
    (when (string= kwop "then")
      (tuareg-find-then-match)
      (tuareg-find-match))
    kwop))

(defun tuareg-find-comma-match ()
  (car (tuareg-find-kwop-pos tuareg-find-comma-match-regexp nil t)))

(defun tuareg-find-pipe-bang-match ()
  (destructuring-bind (kwop pos)
      (tuareg-find-kwop-pos tuareg-find-pipe-bang-match-regexp nil t)
    ;; when matched "if ... then", kwop is "then" but point is at "if"
    (goto-char pos)   ; go back to kwop for tuareg-indent-to-code
    (if (looking-at "\\[|") "[|" kwop)))

(defun tuareg-monadic-operator-p (word)
  (and (or (string= ">>=" word) (string= ">>|" word) (string= ">>>" word))
       word))

(defun tuareg-ignorable-arrow-p ()
  (save-excursion
    (or (tuareg-monadic-operator-p (tuareg-find-arrow-match))
        (looking-at (tuareg-give-extra-unindent-regexp)))))

(defun tuareg-find-monadic-match ()
  (let (kwop)
    (while (or (null kwop)
               (and (string= kwop "=") (tuareg-in-monadic-op-p)))
      (when kwop (tuareg-backward-char 2))
      (setq kwop (tuareg-find-kwop tuareg-find-monadic-match-regexp))
      (when (and (string= kwop "->") (tuareg-ignorable-arrow-p))
        (setq kwop nil)))
    kwop))

(defun tuareg-find-with-match ()
  (tuareg-find-kwop tuareg-find-with-match-regexp))

(defun tuareg-find-in-match ()
  (let ((kwop (tuareg-find-kwop tuareg-find-in-match-regexp "\\<and\\>")))
    (cond
     ((string= kwop "and")
      (tuareg-find-in-match))
     (t
      kwop))))

(defconst tuareg-find-arrow-match-regexp-ls3
  (concat tuareg-find-arrow-match-regexp tuareg-ls3-extras))
(defun tuareg-give-find-arrow-match-regexp ()
  (if (tuareg-editing-ls3)
      tuareg-find-arrow-match-regexp-ls3
    tuareg-find-arrow-match-regexp))

(defconst tuareg-find-then-match-skip-regexp-ls3
  (regexp-opt '("->" "unless" "until") t))
(defconst tuareg-find-then-match-regexp-ls3
  (tuareg-make-find-kwop-regexp tuareg-find-then-match-skip-regexp-ls3))
(defconst tuareg-find-then-match-regexp
  (tuareg-make-find-kwop-regexp "\\(->\\)"))
(defun tuareg-find-then-kwop ()
  (let ((ls3 (tuareg-editing-ls3)))
    (tuareg-find-kwop
     (if ls3 tuareg-find-then-match-regexp-ls3 tuareg-find-then-match-regexp)
     (if ls3 tuareg-find-then-match-regexp-ls3 "\\(->\\)"))))
(defun tuareg-find-then-match ()
  (let ((kwop (tuareg-find-then-kwop)))
    (cond ((string= kwop "if")
           (let ((back (point)))
             (tuareg-back-to-paren-or-indentation)
             (if (looking-at "else[ \t]*\\((\\*.*\\*)\\)*[ \t]*if")
                 "else if"
               (goto-char back)
               kwop)))
          (t kwop))))

(defun tuareg-find-then-else-match ()
  (let ((kwop (tuareg-find-then-kwop)))
    (cond
     ((string= kwop "if")
      (let ((pos (point)))
        (if (and (not (tuareg-in-indentation-p))
                 (string= "else" (tuareg-find-meaningful-word)))
            "else"
          (goto-char pos)
          kwop)))
     (t
      kwop))))

(defun tuareg-find-else-match ()
  (let ((kwop (tuareg-find-kwop tuareg-find-else-match-regexp
                                "\\<then\\>")))
    (cond
     ((string= kwop "then")
      (tuareg-find-then-else-match))
     ((string= kwop ";")
      (tuareg-find-semicolon-match)
      (tuareg-find-else-match)))))

(defconst tuareg-do-match-stop-regexp (tuareg-ro "downto"))
(defun tuareg-find-do-match ()
  (let ((kwop (tuareg-find-kwop tuareg-find-do-match-regexp
                                tuareg-do-match-stop-regexp)))
    (if (or (string= kwop "to") (string= kwop "downto"))
        (tuareg-find-match)
      kwop)))

(defconst tuareg-done-match-stop-regexp (tuareg-ro "and" "do"))
(defun tuareg-find-done-match ()
  (let ((kwop (tuareg-find-kwop (tuareg-give-find-kwop-regexp)
                                tuareg-done-match-stop-regexp)))
    (cond
     ((string= kwop "and")
      (tuareg-find-and-match))
     ((string= kwop "done")
      (tuareg-find-done-match)
      (tuareg-find-done-match))
     ((string= kwop "do")
      (tuareg-find-do-match))
     (t
      kwop))))

(defconst tuareg-and-stop-regexp-ls3 (tuareg-ro "and" "do" "where"))
(defun tuareg-give-and-stop-regexp ()
  (if (tuareg-editing-ls3)
      tuareg-and-stop-regexp-ls3
    "\\<and\\>"))

(defun tuareg-find-and-match ()
  (let* ((kwop (tuareg-find-kwop
                tuareg-find-and-match-regexp
                (tuareg-give-and-stop-regexp)))
         (old-point (point)))
    (cond
     ((or (string= kwop "type") (string= kwop "module"))
      (let ((kwop2 (tuareg-find-meaningful-word)))
        (cond ((string= kwop2 "with")
               kwop2)
              ((string= kwop2 "and")
               (tuareg-find-and-match))
              ((and (string= kwop "module")
                    (string= kwop2 "let"))
               kwop2)
              (t (goto-char old-point) kwop))))
     (t kwop))))

(defconst tuareg-=-stop-regexp-ls3
  (concat (tuareg-ro "and" "do" "in" "where") "\\|="))
(defconst tuareg-=-stop-regexp (concat (tuareg-ro "and" "in") "\\|="))
(defun tuareg-give-=-stop-regexp ()
  (if (tuareg-editing-ls3)
      tuareg-=-stop-regexp-ls3
    tuareg-=-stop-regexp))

(defun tuareg-find-=-match ()
  (let ((kwop (tuareg-find-kwop
               tuareg-find-=-match-regexp
               (tuareg-give-=-stop-regexp))))
    (cond
     ((string= kwop "and")
      (tuareg-find-and-match))
     ((and (string= kwop "=")
           (not (tuareg-false-=-p)))
      (while (and (string= kwop "=")
                  (not (tuareg-false-=-p)))
        (setq kwop (tuareg-find-=-match)))
      kwop)
     (t kwop))))

(defconst tuareg-if-when-regexp (tuareg-ro "if" "when"))
(defun tuareg-if-when-= ()
  (save-excursion
    (tuareg-find-=-match)
    (looking-at tuareg-if-when-regexp)))

(defconst tuareg-captive-regexp
  (tuareg-ro "let" "if" "when" "module" "type" "class"))
(defun tuareg-captive-= ()
  (save-excursion
    (tuareg-find-=-match)
    (looking-at tuareg-captive-regexp)))

(defconst tuareg-pipe-stop-regexp
  (concat (tuareg-ro "and" "with") "\\||"))
(defconst tuareg-pipe-stop-regexp-ls3
  (concat tuareg-pipe-stop-regexp tuareg-ls3-extras))
(defun tuareg-give-pipe-stop-regexp ()
  (if (tuareg-editing-ls3)
      tuareg-pipe-stop-regexp-ls3
    tuareg-pipe-stop-regexp))

(defun tuareg-find-pipe-match ()
  (let ((kwop
         (let ((k (tuareg-find-kwop
                   tuareg-find-pipe-match-regexp
                   (tuareg-give-pipe-stop-regexp))))
           (if (and k (string-match "|[^!]" k))
               "|" k)))
        (old-point (point)))
    (cond
     ((string= kwop "and")
      (setq old-point (point))
      (setq kwop (tuareg-find-and-match))
      (if (not (string= kwop "do"))
          (goto-char old-point)
        (setq kwop (tuareg-find-arrow-match)))
      kwop)
     ((and (string= kwop "|")
           (looking-at "|[^|]")
           (tuareg-in-indentation-p))
      kwop)
     ((string= kwop "|") (tuareg-find-pipe-match))
     ((and (string= kwop "=")
           (or (looking-at (tuareg-no-code-after "="))
               (tuareg-false-=-p)
               (not (string= (save-excursion (tuareg-find-=-match))
                             "type"))))
      (tuareg-find-pipe-match))
     (t
      kwop))))

(defun tuareg-find-arrow-match ()
  (let ((kwop (tuareg-find-kwop (tuareg-give-find-arrow-match-regexp)
                                "\\<with\\>")))
    (cond
     ((string= kwop "|")
      (if (tuareg-in-indentation-p)
          kwop
        (progn (forward-char -1) (tuareg-find-arrow-match))))
     ((string= kwop "fun")
      (let ((pos (point)))
        (or (tuareg-monadic-operator-p (tuareg-find-meaningful-word))
            (progn (goto-char pos) kwop))))
     ((not (string= kwop ":"))
      kwop)
     ;; If we get this far, we know we're looking at a colon.
     ((or (char-equal (char-before) ?:)
          (char-equal (char-after (1+ (point))) ?:)
          (char-equal (char-after (1+ (point))) ?>))
      (tuareg-find-arrow-match))
     ;; Patch by T. Freeman
     (t
      (let ((oldpoint (point))
            (match (tuareg-find-arrow-match)))
        (if (looking-at ":")
            match
          (progn
            ;; Go back to where we were before the recursive call.
            (goto-char oldpoint)
            kwop)))))))

(defconst tuareg-semicolon-match-stop-regexp
  (tuareg-ro "and" "do" "end" "in" "with"))
(defconst tuareg-no-code-after-paren-regexp
  (tuareg-no-code-after "[[{(][|<]?"))
(defun tuareg-semicolon-indent-kwop-point (&optional leading-semi-colon)
  ;; return (kwop kwop-point indentation)
  (let ((kwop (tuareg-find-kwop tuareg-find-semicolon-match-regexp
                                tuareg-semicolon-match-stop-regexp))
        (point (point)))
    ;; We don't need to find the keyword matching `and' since we know
    ;; it's `let'!
    (list
     (cond
       ((string= kwop ";")
        (forward-line 1)
        (while (or (tuareg-in-comment-p)
                   (looking-at tuareg-no-code-this-line-regexp))
          (forward-line 1))
        (back-to-indentation)
        (current-column))
       ((and leading-semi-colon
             (looking-at "\\((\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
             (not (looking-at tuareg-no-code-after-paren-regexp)))
        (current-column))
       ;; ((looking-at (tuareg-no-code-after "\\((\\|\\[[<|]?\\|{<?\\)"))
       ;;  (+ (current-column) tuareg-default-indent))
       ((looking-at (tuareg-no-code-after
                     "\\<begin\\>\\|\\((\\|\\[[<|]?\\|{<?\\)"))
        (if (tuareg-in-indentation-p)
            (+ (current-column) tuareg-default-indent)
          (tuareg-indent-from-previous-kwop)))
       ((looking-at "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)") ; paren with subsequent text
        (tuareg-search-forward-paren)
        (current-column))
       ((string= kwop "method")
        (+ (tuareg-paren-or-indentation-column) tuareg-method-indent))
       ((string= kwop "->")
        (if (save-excursion
              (tuareg-find-arrow-match)
              (or (looking-at "\\<fun\\>\\||")
                  (looking-at (tuareg-give-extra-unindent-regexp))))
            (tuareg-paren-or-indentation-indent)
          (tuareg-find-semicolon-match)))
       ((string= kwop "end")
        (tuareg-find-match)
        (tuareg-find-semicolon-match))
       ((string= kwop "in")
        (tuareg-find-in-match)
        (+ (current-column) tuareg-in-indent))
       ((string= kwop "where")
        (tuareg-find-in-match)
        (+ (tuareg-paren-or-indentation-column) tuareg-in-indent))
       ((string= kwop "let")
        (+ (current-column) tuareg-let-indent))
       ((string= kwop "try")
        (forward-char 3) (skip-syntax-forward " ")
        (current-column))
       (t (tuareg-paren-or-indentation-indent)))
     kwop point)))

(defun tuareg-find-semicolon-match (&optional leading-semi-colon)
  (car (tuareg-semicolon-indent-kwop-point leading-semi-colon)))

(defmacro tuareg-reset-and-kwop (kwop)
  `(when (and ,kwop (string= ,kwop "and"))
     (setq ,kwop (tuareg-find-and-match))))

(defconst tuareg-phrase-regexp-1 (tuareg-ro "module" "type"))
(defconst tuareg-phrase-regexp-2 (tuareg-ro "and" "let" "module" "with"))
(defconst tuareg-phrase-regexp-3
  (tuareg-ro "and" "end" "every" "in" "with"))
(defun tuareg-find-phrase-indentation (&optional phrase-break)
  (if (and (looking-at tuareg-phrase-regexp-1) (> (point) (point-min))
           (save-excursion
             (tuareg-find-meaningful-word)
             (looking-at tuareg-phrase-regexp-2)))
      (progn
        (tuareg-find-meaningful-word)
        (+ (current-column) tuareg-default-indent))
    (let ((looking-at-and (looking-at "\\<and\\>"))
          (kwop (tuareg-find-kwop
                 (if phrase-break
                     tuareg-find-phrase-indentation-break-regexp
                   tuareg-find-phrase-indentation-regexp)
                 tuareg-phrase-regexp-3))
          (tmpkwop nil) (curr nil))
      (tuareg-reset-and-kwop kwop)
      (cond ((not kwop) (current-column))
            ((string= kwop "every")
             (if (tuareg-editing-ls3)
                 (progn
                   (tuareg-find-done-match)
                   (tuareg-find-phrase-indentation phrase-break)
                   (current-column))
               (tuareg-find-phrase-indentation phrase-break)))
            ((string= kwop "end")
             (if (not (save-excursion
                        (setq tmpkwop (tuareg-find-match))
                        (setq curr (point))
                        (string= tmpkwop "object")))
                 (progn
                   (tuareg-find-match)
                   (tuareg-find-phrase-indentation phrase-break))
               (tuareg-find-kwop tuareg-find-phrase-indentation-class-regexp)
               (current-column)))
            ((and (string= kwop "with")
                  (not (save-excursion
                         (setq tmpkwop (tuareg-find-with-match))
                         (setq curr (point))
                         (string= tmpkwop "module"))))
             (goto-char curr)
             (tuareg-find-phrase-indentation phrase-break))
            ((and (string= kwop "in")
                  (not (save-excursion
                         (setq tmpkwop (tuareg-find-in-match))
                         (tuareg-reset-and-kwop tmpkwop)
                         (setq curr (point))
                         (and (string= tmpkwop "let")
                              (not (tuareg-looking-at-internal-let))))))
             (goto-char curr)
             (tuareg-find-phrase-indentation phrase-break))
            ((tuareg-at-phrase-break-p)
             (end-of-line)
             (tuareg-skip-blank-and-comments)
             (current-column))
            ((string= kwop "let")
             (if (tuareg-looking-at-internal-let)
                 (tuareg-find-phrase-indentation phrase-break)
                 (current-column)))
            ((string= kwop "with")
             (current-column))
            ((string= kwop "end")
             (current-column))
            ((or (string= kwop "in") (string= kwop "where"))
             (tuareg-find-in-match)
             (current-column))
            ((string= kwop "class")
             (tuareg-paren-or-indentation-column))
            ((looking-at tuareg-inside-module-or-class-opening)
             (+ (tuareg-paren-or-indentation-column)
                (tuareg-assoc-indent kwop)))
            ((or (string= kwop "type") (string= kwop "module"))
             (if (or (tuareg-looking-at-false-type)
                     (tuareg-looking-at-false-module))
                 (if looking-at-and
                     (current-column)
                   (if (string= "and" (tuareg-find-meaningful-word))
                       (progn
                         (tuareg-find-and-match)
                         (tuareg-find-phrase-indentation phrase-break))
                     (tuareg-find-phrase-indentation phrase-break)))
               (current-column)))
            ((looking-at "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*[^ \t\n]")
             (tuareg-search-forward-paren)
             (current-column))
            ((string= kwop "open") ; compatible with Caml Light `#open'
             (tuareg-paren-or-indentation-column))
            (t (current-column))))))

(defconst tuareg-paren-or-indentation-stop-regexp
  (tuareg-ro "and" "do" "in" "with"))
(defun tuareg-back-to-paren-or-indentation ()
  "Search backwards for the first open paren in line, or skip to indentation.
Returns t iff skipped to indentation."
  (if (or (bolp) (tuareg-in-indentation-p))
      (progn (back-to-indentation) t)
    (let ((kwop (tuareg-find-kwop
                 tuareg-back-to-paren-or-indentation-regexp
                 tuareg-paren-or-indentation-stop-regexp))
          (retval))
      (when (string= kwop "with")
        (let ((with-point (point)))
          (setq kwop (tuareg-find-with-match))
          (if (or (string= kwop "match") (string= kwop "try"))
              (tuareg-find-kwop tuareg-back-to-paren-or-indentation-regexp
                                "\\<and\\>")
            (setq kwop "with") (goto-char with-point))))
      (setq retval
            (cond
             ((string= kwop "with") nil)
             ((or (string= kwop "in") (string= kwop "do"))
              (tuareg-in-indentation-p))
             ;; ((looking-at "[[{(]") (tuareg-search-forward-paren) nil)
             ;; ((looking-at "\\.<")
             ;;  (if tuareg-support-metaocaml
             ;;      (progn
             ;;        (tuareg-search-forward-paren) nil)
             ;;    (tuareg-back-to-paren-or-indentation)))
             (t (back-to-indentation) t)))
      (cond
       ;; ((looking-at "|[^|]")
       ;;  (re-search-forward "|[^|][ \t]*") nil)
       ((or (string= kwop "in") (string= kwop "do"))
        (tuareg-find-in-match)
        (tuareg-back-to-paren-or-indentation)
        (if (looking-at "\\<\\(let\\|and\\)\\>")
            (forward-char tuareg-in-indent)) nil)
       (t retval)))))

(defun tuareg-paren-or-indentation-column ()
  (tuareg-back-to-paren-or-indentation)
  (current-column))

(defun tuareg-paren-or-indentation-indent ()
  (+ (tuareg-paren-or-indentation-column) tuareg-default-indent))

(defun tuareg-search-forward-paren ()
  (re-search-forward "\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)[ \t]*"))

(defun tuareg-add-default-indent (leading-operator)
  (if leading-operator 0 tuareg-default-indent))

(defconst tuareg-internal-let-regexp
  (concat "[[({;=]\\|"
           (tuareg-ro "begin" "open" "if" "in" "do" "try" "then" "else"
                      "match" "while" "when")))
(defun tuareg-looking-at-internal-let ()
  (save-excursion
    (tuareg-find-meaningful-word)
    (and (not (tuareg-at-phrase-break-p))
         (not (and tuareg-support-metaocaml
                   (char-equal ?. (following-char))
                   (char-equal ?> (preceding-char))))
         (or (looking-at tuareg-internal-let-regexp)
             (looking-at tuareg-operator-regexp)))))

(defconst tuareg-false-module-regexp (tuareg-ro "and" "let" "with"))
(defun tuareg-looking-at-false-module ()
  (save-excursion
    (tuareg-find-meaningful-word)
    (looking-at tuareg-false-module-regexp)))

(defun tuareg-looking-at-false-sig-struct ()
  (save-excursion
    (tuareg-find-module)
    (looking-at "\\<module\\>\\|(")))

(defconst tuareg-false-type-regexp (tuareg-ro "and" "class" "module" "with"))
(defun tuareg-looking-at-false-type ()
  (save-excursion
    (tuareg-find-meaningful-word)
    (looking-at tuareg-false-type-regexp)))

(defun tuareg-looking-at-in-let ()
  (save-excursion
    (string= (tuareg-find-meaningful-word) "in")))

(defun tuareg-find-module ()
  (tuareg-find-kwop tuareg-find-module-regexp))

(defun tuareg-indent-from-previous-kwop ()
  (let* ((start-pos (point))
         (kwop (tuareg-find-argument-kwop-non-blank t))
         (captive= (and (string= kwop "=") (tuareg-captive-=)))
         (kwop-pos (point)))
    (forward-char (length kwop))
    (tuareg-skip-blank-and-comments)
    (cond ((or (not captive=)
               (/= (point) start-pos)) ; code between paren and kwop
           (goto-char start-pos)
           (tuareg-paren-or-indentation-indent))
          (t
           (goto-char kwop-pos)
           (when (string= kwop "=")
             (setq kwop (tuareg-find-=-match)))
           (+ tuareg-default-indent
              (if (assoc kwop tuareg-leading-kwop-alist)
                  (tuareg-compute-kwop-indent kwop)
                  (current-column)))))))

(defun tuareg-find-colon-typespec (start-pos)
  (let* ((old-pos (point))
         (new-pos (search-forward ":" start-pos t)))
    (when new-pos
      (backward-char 1)
      (skip-syntax-backward " ")
      (skip-syntax-backward "w")
      (skip-syntax-backward " ")
      (let ((char (char-before)))
        (cond ((or (char-equal char ??) (char-equal char ?~))
               (goto-char old-pos) nil)
              (t (goto-char new-pos) t))))))

(defun tuareg-indent-from-paren (leading-operator start-pos)
  (cond
   ((looking-at (tuareg-no-code-after "\\(\\(\\.<\\|(\\|\\[[<|]?\\|{<?\\)\\)"))
    (cond ((tuareg-in-indentation-p)
           (+ tuareg-default-indent
              (current-column)))
          ((tuareg-find-colon-typespec start-pos)
           (if (looking-at tuareg-no-code-this-line-regexp)
               (tuareg-paren-or-indentation-indent)
             (tuareg-skip-blank-and-comments)
             (current-column)))
          (t (tuareg-indent-from-previous-kwop))))
   ((looking-at "\\<begin\\>")
    (tuareg-paren-or-indentation-indent))
   ((looking-at "([ \t]*\\(\\w\\)")
    (goto-char (match-beginning 1))
    (current-column))
   (t
    (+ (tuareg-add-default-indent leading-operator)
       (current-column)))))

(defun tuareg-skip-to-next-form (old-point)
  (while (and (not (looking-at tuareg-no-more-code-this-line-regexp))
              (< (point) old-point)) ; do not go beyond old-point
    (forward-sexp 1))
  (tuareg-skip-blank-and-comments)
  (tuareg-back-to-paren-or-indentation))

(defun tuareg-find-argument-kwop (leading-operator)
  (tuareg-find-kwop (if leading-operator
                      tuareg-compute-argument-indent-regexp
                      tuareg-compute-normal-indent-regexp)
                    (tuareg-give-keyword-regexp)))

(defun tuareg-find-argument-kwop-clean (leading-operator)
  (let (kwop)
    (while (or (progn (setq kwop (tuareg-find-argument-kwop leading-operator))
                      (tuareg-reset-and-kwop kwop)
                      nil)
               (and (string= kwop "=") (tuareg-false-=-p))
               (and (looking-at tuareg-no-code-this-line-regexp)
                    (not (= (point) (point-min))))))
    kwop))

(defun tuareg-find-argument-kwop-non-blank (leading-operator)
  (let ((kwop "") (point (1+ (point))))
    (while (and (> point (point)) (string= "" kwop))
      (setq point (point)
            kwop (tuareg-find-argument-kwop-clean leading-operator)))
    kwop))

(defun tuareg-compute-argument-indent (leading-operator)
  (let* ((old-point (line-beginning-position))
         (kwop (tuareg-find-argument-kwop-non-blank leading-operator))
         (match-end-point (+ (point) (length kwop)))) ; match-end is invalid!
    (cond
     ((and (string= kwop "->")
           (not (looking-at (tuareg-no-code-after "->"))))
      (let (matching-kwop matching-pos)
        (save-excursion
          (setq matching-kwop (tuareg-find-arrow-match))
          (setq matching-pos (point)))
        (cond
         ((string= matching-kwop ":")
          (goto-char matching-pos)
          (tuareg-find-arrow-match) ; matching `val' or `let'
          (+ (current-column) tuareg-val-indent))
         ((or (string= matching-kwop "val") (string= matching-kwop "let"))
          (+ (current-column) tuareg-val-indent))
         ((string= matching-kwop "|")
          (goto-char matching-pos)
          (+ (tuareg-add-default-indent leading-operator)
             (current-column)
             (- tuareg-pipe-extra-unindent)
             tuareg-default-indent))
         (t
          (+ (tuareg-paren-or-indentation-column)
             (tuareg-add-default-indent leading-operator))))))
     ((string= kwop "fun")
      (+ (tuareg-paren-or-indentation-column)
         (tuareg-add-default-indent leading-operator)
         (tuareg-assoc-indent kwop)))
     ((<= old-point (point))
      (+ (tuareg-add-default-indent leading-operator)
         (current-column)))
     (t
      (goto-char match-end-point) ; skip kwop == (forward-char (length kwop))
      (tuareg-skip-to-next-form old-point)
      (+ (tuareg-add-default-indent
          (if (save-excursion (goto-char match-end-point)
                              (looking-at tuareg-no-more-code-this-line-regexp))
              (or leading-operator (string= kwop "{")
                  (looking-at (tuareg-no-code-after "[[:upper:]].*\\.")))
            (not (looking-at tuareg-operator-regexp))))
         (current-column))))))

(defun tuareg-compute-arrow-indent (start-pos)
  (let (kwop pos)
    (save-excursion (setq kwop (tuareg-find-arrow-match) pos (point)))
    (cond ((string= kwop "|")
           (tuareg-find-arrow-match)
           (+ (current-column) tuareg-default-indent))
          ((or (string= kwop "val")
               (string= kwop "let"))
           (goto-char pos)
           (+ (current-column) tuareg-val-indent))
          ((string= kwop "type")
           (goto-char pos)
           (+ (current-column) tuareg-type-indent
              tuareg-default-indent))
          ((string= kwop "(")
           (goto-char pos)
           (tuareg-indent-after-next-char))
          ((or (string= kwop "{")
               (string= kwop ";"))
           (if (and (looking-at "->")
                    (search-backward ":" pos t))
               (tuareg-indent-after-next-char)
             (tuareg-back-to-paren-or-indentation)
             (current-column)))
          ((tuareg-monadic-operator-p kwop)
           ;; find the last ">>=" or ">>>"
           ;; (goto-char pos)
           ;; (let ((back (point)))
           ;;   (while (tuareg-monadic-operator-p (tuareg-find-arrow-match))
           ;;     (setq back (point)))
           ;;   (goto-char back))
           ;; (if (not (re-search-backward
           ;;           (concat "(\\|" (tuareg-give-keyword-regexp))
           ;;           (point-min) t))
           ;;     0
           ;;   (goto-char (match-end 0))
           ;;   (tuareg-skip-blank-and-comments)
           ;;   (tuareg-compute-indent))

           ;; this is not perfect, in particular, inside match.
           ;; (see example in sample.ml)
           ;; the problem is that we cannot skip an expression backwards.
           ;; workaround: wrap code in parens
           (destructuring-bind (indent kwop point)
               (tuareg-semicolon-indent-kwop-point)
             (- indent
                (if (string= kwop "in")
                    tuareg-in-indent 0))))
          (t (tuareg-paren-or-indentation-indent)))))

(defun tuareg-compute-keyword-indent (kwop leading-operator start-pos)
  (cond ((string= kwop ";")
         (if (looking-at (tuareg-no-code-after ";"))
             (let* ((pos (point)) (indent (tuareg-find-semicolon-match)))
               (if (looking-at tuareg-phrase-regexp-1)
                   (progn
                     (goto-char start-pos)
                     (if (search-backward ":" pos t)
                         (tuareg-indent-after-next-char)
                       indent))
                 indent))
           (tuareg-paren-or-indentation-indent)))
        ((string= kwop ",")
         (if (looking-at (tuareg-no-code-after ","))
             (let ((mkwop (tuareg-find-comma-match)))
               (cond ((or (string= mkwop "[")
                          (string= mkwop "{")
                          (string= mkwop "("))
                      (forward-char 1) (skip-syntax-forward " ")
                      (current-column))
                     ((looking-at "[[{(]\\|\\.<")
                      (tuareg-indent-from-paren t start-pos))
                     ((or (and (looking-at "[<|]")
                               (char-equal ?\[ (preceding-char)))
                          (and (looking-at "<")
                               (char-equal ?\{ (preceding-char))))
                      (tuareg-backward-char)
                      (tuareg-indent-from-paren t start-pos))
                     ((and (looking-at "\\<let\\>") (string= mkwop "in"))
                      (+ (current-column) tuareg-in-indent))
                     (t (+ (tuareg-paren-or-indentation-column)
                           (tuareg-assoc-indent mkwop)))))
           (tuareg-paren-or-indentation-indent)))
        ((looking-at "\\<begin\\>\\|->")
         (if (looking-at (tuareg-no-code-after "\\(\\<begin\\>\\|->\\)"))
             (tuareg-indent-from-paren leading-operator start-pos)
           (+ tuareg-default-indent
              (tuareg-indent-from-paren leading-operator start-pos))))
        ((or (string= kwop "let") (string= kwop "and"))
         (tuareg-back-to-paren-or-indentation)
         (+ (tuareg-paren-or-indentation-indent)
            (tuareg-assoc-indent kwop t)))
        ((string= kwop "with")
         (if (save-excursion
               (let ((tmpkwop (tuareg-find-with-match)))
                 (or (string= tmpkwop "module")
                     (string= tmpkwop "{"))))
             (tuareg-paren-or-indentation-indent)
           (+ (tuareg-paren-or-indentation-column)
              (* 2 tuareg-default-indent) ; assume a missing first "|"
              (tuareg-assoc-indent kwop t))))
        ((string-match "\\<\\(fun\\|of\\)\\>" kwop)
         (+ (tuareg-paren-or-indentation-column)
            (tuareg-add-default-indent leading-operator)
            (tuareg-assoc-indent kwop t)))
        ((string-match (tuareg-give-extra-unindent-regexp) kwop)
         (+ (tuareg-paren-or-indentation-column)
            (tuareg-assoc-indent kwop t)))
        ((string= kwop "in")
         (when (looking-at (tuareg-no-code-after "\\<in\\>"))
           (tuareg-find-in-match))
         (+ (current-column)
            tuareg-in-indent))
        ((string-match (tuareg-give-matching-kwop-regexp) kwop)
         (tuareg-find-leading-kwop-match kwop)
         (if (tuareg-in-indentation-p)
             (+ (current-column)
                (tuareg-assoc-indent kwop t))
           (tuareg-back-to-paren-or-indentation)
           (+ (tuareg-paren-or-indentation-indent)
              (tuareg-assoc-indent kwop t))))
        ((string= kwop "try")
         (forward-char 3)
         (if (looking-at tuareg-no-more-code-this-line-regexp)
             (+ (current-column) -3 tuareg-default-indent)
           (skip-syntax-forward " ")
           (+ (current-column) tuareg-default-indent)))
        (t (+ (if (tuareg-in-indentation-p)
                  (current-column)
                (tuareg-paren-or-indentation-indent))
              (tuareg-assoc-indent kwop t)))))

(defconst tuareg-=-indent-regexp-1
  (tuareg-ro "val" "let" "method" "module" "class" "when" "for" "if" "do"))

(defun tuareg-compute-=-indent (start-pos)
  (let ((current-column-module-type nil) (kwop1 (tuareg-find-=-match))
        (next-pos (point)))
    (+ (save-excursion
         (tuareg-reset-and-kwop kwop1)
         (cond ((string= kwop1 "type")
                (tuareg-find-meaningful-word)
                (cond ((looking-at "\\<module\\>")
                       (setq current-column-module-type (current-column))
                       tuareg-default-indent)
                      ((looking-at "\\<\\(with\\|and\\)\\>")
                       (tuareg-find-with-match)
                       (setq current-column-module-type (current-column))
                       tuareg-default-indent)
                      (t (goto-char start-pos)
                         (beginning-of-line)
                         (+ (tuareg-add-default-indent
                             (looking-at "[ \t]*[\[|]"))
                            tuareg-type-indent))))
               ((looking-at tuareg-=-indent-regexp-1)
                (let ((matched-string (tuareg-match-string 0)))
                  (setq current-column-module-type (current-column))
                  (tuareg-assoc-indent matched-string)))
               ((looking-at "\\<object\\>")
                (tuareg-back-to-paren-or-indentation)
                (setq current-column-module-type (current-column))
                (+ (tuareg-assoc-indent "object")
                   tuareg-default-indent))
               ((looking-at tuareg-no-code-after-paren-regexp)
                (setq current-column-module-type
                      (tuareg-indent-from-paren nil next-pos))
                tuareg-default-indent)
               (t (setq current-column-module-type
                        (tuareg-paren-or-indentation-indent))
                  tuareg-default-indent)))
       (or current-column-module-type
           (current-column)))))

(defun tuareg-indent-after-next-char ()
  (forward-char 1)
  (tuareg-skip-blank-and-comments)
  (current-column))

(defconst tuareg-definitions-regexp
  (tuareg-ro "and" "val" "type" "module" "class" "exception" "let")
  "Regexp matching definition phrases.")

(defun tuareg-compute-normal-indent ()
  (let ((leading-operator (looking-at tuareg-operator-regexp)))
    (beginning-of-line)
    (save-excursion
      (let ((start-pos (point))
            (kwop (tuareg-find-argument-kwop-clean leading-operator)))
        (cond
          ((not kwop) (current-column))
          ((tuareg-at-phrase-break-p)
           (tuareg-find-phrase-indentation t))
          ((and (string= kwop "|") (not (char-equal ?\[ (preceding-char))))
           (tuareg-backward-char)
           (+ (tuareg-paren-or-indentation-indent)
              (tuareg-add-default-indent leading-operator)))
          ((or (looking-at "[[{(]")
               (and (looking-at "[<|]")
                    (char-equal ?\[ (preceding-char))
                    (progn (tuareg-backward-char) t))
               (and (looking-at "<")
                    (char-equal ?\{ (preceding-char))
                    (progn (tuareg-backward-char) t)))
           (cond ((looking-at "{ *[A-Z]")
                  (forward-char 1) (skip-syntax-forward " ")
                  (current-column))
                 ((looking-at (tuareg-no-code-after "[[{(][<|]?"))
                  (tuareg-indent-from-paren leading-operator start-pos))
                 ((and leading-operator (string= kwop "("))
                  (tuareg-indent-after-next-char))
                 (t (+ tuareg-default-indent
                       (tuareg-indent-from-paren leading-operator
                                                 start-pos)))))
          ((looking-at "\\.<")
           (if (looking-at (tuareg-no-code-after "\\.<"))
               (tuareg-indent-from-paren leading-operator start-pos)
             (+ tuareg-default-indent
                (tuareg-indent-from-paren leading-operator start-pos))))
          ((looking-at "->")
           (tuareg-compute-arrow-indent start-pos))
          ((looking-at (tuareg-give-keyword-regexp))
           (tuareg-compute-keyword-indent kwop leading-operator start-pos))
          ((and (string= kwop "=") (not (tuareg-false-=-p))
                (or (null leading-operator)
                    ;; defining "=", not testing for equality
                    (string-match tuareg-definitions-regexp
                                  (save-excursion
                                    (tuareg-find-argument-kwop-clean t)))))
           (tuareg-compute-=-indent start-pos))
          (nil 0)
          (t (tuareg-compute-argument-indent leading-operator)))))))

(defun tuareg-compute-pipe-indent (matching-kwop old-point)
  (cond
    ((string= matching-kwop "|")
     (tuareg-back-to-paren-or-indentation)
     (current-column))
    ((and (string= matching-kwop "=")
          (not (tuareg-false-=-p)))
     (re-search-forward "=[ \t]*")
     (current-column))
    ((and matching-kwop
          (looking-at (tuareg-give-match-pipe-kwop-regexp)))
     (when (looking-at (tuareg-give-extra-unindent-regexp))
       (tuareg-back-to-paren-or-indentation))
     (+ (tuareg-assoc-indent matching-kwop t)
        (tuareg-add-default-indent (not (looking-at "|")))
        (current-column)
        (if (or (string= matching-kwop "type")
                (string= matching-kwop "["))
            0
            tuareg-pipe-extra-unindent)))
    (t
     (goto-char old-point)
     (tuareg-compute-normal-indent))))

(defun tuareg-compute-paren-indent (paren-match-p old-point)
  (unless paren-match-p
    (tuareg-search-forward-paren))
  (let ((looking-at-paren (char-equal ?\( (char-after))) (start-pos (point)))
    (when (or looking-at-paren
              (looking-at (tuareg-no-code-after "\\(\{\\(.*with[ \t]*\\([[:upper:]].*\\.\\)?\\)?\\|\\[\\)")))
      (if (or (tuareg-in-indentation-p)
              (save-excursion (string= ":" (tuareg-find-meaningful-word))))
          (tuareg-back-to-paren-or-indentation)
        (tuareg-indent-from-previous-kwop))
      (when looking-at-paren
        (skip-chars-forward "( \t" start-pos))
      (while (and (looking-at "[([{]")
                  (> (scan-sexps (point) 1)
                     (save-excursion (goto-char old-point)
                                     (line-end-position))))
        (forward-char 1)
        (skip-syntax-forward " "))))
  (current-column))

(defun tuareg-compute-kwop-indent-general (kwop matching-kwop)
  (let* ((looking-at-matching (looking-at matching-kwop))
         (extra-unindent        ; non-paren code before matching-kwop
          (unless (save-excursion
                    (skip-chars-backward "( \t" (line-beginning-position))
                    (bolp))
            (tuareg-back-to-paren-or-indentation)
            t)))
    (+ (current-column)
       (tuareg-add-default-indent
        (if extra-unindent
            (or (string= matching-kwop "struct")
                (string= matching-kwop "object")
                (string= matching-kwop "with")
                (string= kwop "end"))
            (or (not (string= kwop "then"))
                looking-at-matching))))))

(defun tuareg-compute-kwop-indent (kwop)
  (when (string= kwop "rec")
    (setq kwop "and"))
  (let* ((old-point (point))
         (paren-match-p (looking-at "[|>]?[]})]\\|>\\."))
         (real-pipe (looking-at "|\\([^|]\\|$\\)"))
         (matching-kwop (tuareg-find-leading-kwop-match kwop)))
    (cond ((string= kwop "|")
           (if real-pipe
               (tuareg-compute-pipe-indent matching-kwop old-point)
             (goto-char old-point)
             (tuareg-compute-normal-indent)))
          ((looking-at "[[{(][<|]?\\|\\.<")
           (tuareg-compute-paren-indent paren-match-p old-point))
          ((string= kwop "with")
           (when (string= matching-kwop "type")
             (setq old-point (point)
                   matching-kwop (tuareg-find-meaningful-word)))
           (while (string= matching-kwop "with")
             (tuareg-find-with-match)
             (setq matching-kwop (tuareg-find-leading-kwop-match kwop)))
           (cond ((or (string= matching-kwop "module")
                      (string= matching-kwop "struct"))
                  (tuareg-paren-or-indentation-indent))
                 ((or (string= matching-kwop "try")
                      (string= matching-kwop "match"))
                  (tuareg-compute-kwop-indent-general kwop matching-kwop))
                 (t (goto-char old-point)
                    (tuareg-compute-kwop-indent-general kwop matching-kwop))))
          ((and (tuareg-editing-ls3)
                (or (string= kwop "do")
                    (string= kwop "done")
                    (string= kwop "reset")
                    (string= kwop "unless")
                    (string= kwop "until")))
           (tuareg-back-to-paren-or-indentation)
           (if (string= matching-kwop "->")
               (+ (current-column) tuareg-default-indent)
             (current-column)))
          ((or (and (string= kwop "and")
                    (string= matching-kwop "reset"))
               (and (string= kwop "end")
                    (tuareg-editing-ls3)
                    (or (string= matching-kwop "match")
                        (string= matching-kwop "automaton")
                        (string= matching-kwop "present"))))
           (if (tuareg-in-indentation-p)
               (current-column)
             (tuareg-paren-or-indentation-column)))
          ((string= kwop "in")
           (+ (current-column)
              (tuareg-add-default-indent (string= matching-kwop "let"))))
          ((not (string= kwop "and")) ; pretty general case
           (tuareg-compute-kwop-indent-general kwop matching-kwop))
          ((string= matching-kwop "with")
           (current-column))
          (t (tuareg-paren-or-indentation-column)))))

(defun tuareg-indent-to-code (beg-pos match)
  (unless (and (string= match "(")
               (search-forward "->" beg-pos t))
    (forward-char (length match)))
  (tuareg-skip-blank-and-comments)
  (current-column))

(defun tuareg-indent-command (&optional from-leading-star)
  "Indent the current line in Tuareg mode.

Compute new indentation based on OCaml syntax."
  (interactive "*")
  (unless from-leading-star
    (tuareg-auto-fill-insert-leading-star))
  (let ((case-fold-search nil))
   (tuareg-with-internal-syntax
    (save-excursion
      (back-to-indentation)
      (indent-line-to (max 0 (tuareg-compute-indent))))
    (when (tuareg-in-indentation-p) (back-to-indentation)))))

(defconst tuareg-sig-struct-regexp (tuareg-ro "sig" "struct"))
(defconst tuareg-top-level-command-regexp
  (concat "#" (tuareg-ro "open" "load" "use")))
(defun tuareg-compute-indent ()
  (save-excursion
    (cond
     ((tuareg-in-comment-p)
      (cond
       ((looking-at "(\\*")
        (if tuareg-indent-leading-comments
            (save-excursion
              (tuareg-skip-blank-and-comments)
              (back-to-indentation)
              (current-column))
          (current-column)))
       ((looking-at "\\*\\**)")
        (tuareg-beginning-of-literal-or-comment-fast)
        (if (tuareg-leading-star-p)
            (+ (current-column)
               (if (save-excursion
                     (forward-line 1)
                     (back-to-indentation)
                     (looking-at "*")) 1
                 tuareg-comment-end-extra-indent))
          (+ (current-column) tuareg-comment-end-extra-indent)))
       (tuareg-indent-comments
        (let ((star (and (tuareg-leading-star-p)
                         (looking-at "\\*"))))
          (tuareg-beginning-of-literal-or-comment-fast)
          (if star (re-search-forward "(") (re-search-forward "(\\*+[ \t]*"))
          (current-column)))
       (t (current-column))))
     ((tuareg-in-literal-p)
      (current-column))
     ((or (looking-at "\\<let\\>") (looking-at "\\<open\\>"))
      (if (tuareg-looking-at-internal-let)
          (if (tuareg-looking-at-in-let)
              (progn
                (tuareg-find-meaningful-word)
                (tuareg-find-in-match)
                (current-column))
            (tuareg-compute-normal-indent))
        (tuareg-find-phrase-indentation)))
     ((or (looking-at tuareg-governing-phrase-regexp)
          (looking-at ";;"))
      (tuareg-find-phrase-indentation))
     ((and tuareg-sig-struct-align (looking-at tuareg-sig-struct-regexp))
      (if (string= (tuareg-find-module) "module") (current-column)
        (tuareg-paren-or-indentation-indent)))
     ((looking-at ";")
      (tuareg-find-semicolon-match t))
     ((looking-at "|!")
      (tuareg-indent-to-code (line-beginning-position)
                             (tuareg-find-pipe-bang-match)))
     ((looking-at ">>[=>|]")
      (tuareg-indent-to-code (line-beginning-position)
                             (tuareg-find-monadic-match)))
     ((or (looking-at "%\\|;;")
          (and tuareg-support-camllight (looking-at "#"))
          (looking-at tuareg-top-level-command-regexp))
      0)
     ((or (looking-at (tuareg-give-matching-kwop-regexp))
          (looking-at "\\<rec\\>")
          (and tuareg-support-metaocaml
               (looking-at ">\\.")))
      (tuareg-compute-kwop-indent (tuareg-match-string 0)))
     (t (tuareg-compute-normal-indent)))))

(defun tuareg-split-string ()
  "Called whenever a line is broken inside an OCaml string literal."
  (insert-before-markers "\\ ")
  (tuareg-backward-char))

(defun tuareg-newline-and-indent ()
  "Like `newline-and-indent' but handles OCaml's multi-line strings."
  (interactive)
  (let ((hooked (tuareg-in-literal-p))
        (split-mark))
    (when hooked
      (setq split-mark (set-marker (make-marker) (point)))
      (tuareg-split-string))
    (newline-and-indent)
    (when hooked
      (goto-char (max (point) split-mark))
      (set-marker split-mark nil))))

(defun tuareg-electric-pipe ()
  "If inserting a | operator at beginning of line, reindent the line."
  (interactive "*")
  (let ((electric (and tuareg-electric-indent
                       (not (and (boundp 'electric-indent-mode)
                                 electric-indent-mode))
                       (tuareg-in-indentation-p)
                       (not (tuareg-in-literal-p))
                       (not (tuareg-in-comment-p)))))
    (self-insert-command 1)
    (and electric
         (not (and (char-equal ?| (preceding-char))
                   (save-excursion
                     (tuareg-backward-char)
                     (tuareg-find-pipe-match)
                     (not (looking-at (tuareg-give-match-pipe-kwop-regexp))))))
         (indent-according-to-mode))))

(defun tuareg-electric-rp ()
  "If inserting a ) operator or a comment-end at beginning of line,
reindent the line."
  (interactive "*")
  (let ((electric (and tuareg-electric-indent
                       (not (and (boundp 'electric-indent-mode)
                                 electric-indent-mode))
                       (or (tuareg-in-indentation-p)
                           (char-equal ?* (preceding-char)))
                       (not (tuareg-in-literal-p))
                       (or (not (tuareg-in-comment-p))
                           (save-excursion
                             (back-to-indentation)
                             (looking-at "\\*"))))))
    (self-insert-command 1)
    (and electric
         (indent-according-to-mode))))

(defun tuareg--electric-close-vector ()
  ;; Function for use on post-self-insert-hook.
  (when tuareg-electric-close-vector
    (let ((inners (cdr (assq last-command-event
                             '((?\} ?> "{<") (?\] ?| "\\[|"))))))
      (and inners
           (eq (char-before) last-command-event) ;; Sanity check.
           (not (eq (car inners) (char-before (1- (point)))))
           (not (tuareg-in-literal-or-comment-p))
           (save-excursion
             (when (ignore-errors (backward-sexp 1) t)
               (looking-at (nth 1 inners))))
           (save-excursion
             (goto-char (1- (point)))
             (insert (car inners)))))))

(defun tuareg-electric-rc ()
  "If inserting a } operator at beginning of line, reindent the line.

Reindent also if } is inserted after a > operator at beginning of line.
Also, if the matching { is followed by a < and this } is not preceded
by >, insert one >."
  (interactive "*")
  (let* ((prec (preceding-char))
         (look-bra (and tuareg-electric-close-vector
                        (not (boundp 'post-self-insert-hook))
                        (not (tuareg-in-literal-or-comment-p))
                        (not (char-equal ?> prec))))
         (electric (and tuareg-electric-indent
                        (not (and (boundp 'electric-indent-mode)
                                  electric-indent-mode))
                        (or (tuareg-in-indentation-p)
                            (and (char-equal ?> prec)
                                 (save-excursion (tuareg-backward-char)
                                                 (tuareg-in-indentation-p))))
                        (not (tuareg-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (when look-bra
      (save-excursion
        (let ((inserted-char
               (save-excursion
                 (tuareg-backward-char)
                 (tuareg-backward-up-list)
                 (cond ((looking-at "{<") ">")
                       (t "")))))
          (tuareg-backward-char)
          (insert inserted-char))))
    (when electric (indent-according-to-mode))))

(defun tuareg-electric-rb ()
  "If inserting a ] operator at beginning of line, reindent the line.

Reindent also if ] is inserted after a | operator at beginning of line.
Also, if the matching [ is followed by a | and this ] is not preceded
by |, insert one |."
  (interactive "*")
  (let* ((prec (preceding-char))
         (look-pipe-or-bra (and tuareg-electric-close-vector
                                (not (boundp 'post-self-insert-hook))
                                (not (tuareg-in-literal-or-comment-p))
                                (not (and (char-equal ?| prec)
                                          (not (char-equal
                                                (save-excursion
                                                  (tuareg-backward-char)
                                                  (preceding-char)) ?\[))))))
         (electric (and tuareg-electric-indent
                        (not (and (boundp 'electric-indent-mode)
                                  electric-indent-mode))
                        (or (tuareg-in-indentation-p)
                            (and (char-equal ?| prec)
                                 (save-excursion (tuareg-backward-char)
                                                 (tuareg-in-indentation-p))))
                        (not (tuareg-in-literal-or-comment-p)))))
    (self-insert-command 1)
    (when look-pipe-or-bra
      (save-excursion
        (let ((inserted-char
               (save-excursion
                 (tuareg-backward-char)
                 (tuareg-backward-up-list)
                 (cond ((looking-at "\\[|") "|")
                       (t "")))))
          (tuareg-backward-char)
          (insert inserted-char))))
    (when electric (indent-according-to-mode))))

(defun tuareg-abbrev-hook ()
  "If inserting a leading keyword at beginning of line, reindent the line."
  (unless (or (and (boundp 'electric-indent-mode) electric-indent-mode)
              (tuareg-in-literal-or-comment-p))
    (let* ((bol (line-beginning-position))
           (kw (save-excursion
                 (and (re-search-backward "^[ \t]*\\(\\w\\|_\\)+\\=" bol t)
                      (tuareg-match-string 1)))))
      (when kw
        (insert " ")
        (indent-according-to-mode)
        (backward-delete-char-untabify 1)))))

(defun tuareg-skip-to-end-of-phrase ()
  (let ((old-point (point)))
    (when (and (string= (tuareg-find-meaningful-word) ";")
               (char-equal (preceding-char) ?\;))
      (setq old-point (1- (point))))
    (goto-char old-point)
    (let ((kwop (tuareg-find-meaningful-word)))
      (goto-char (+ (point) (length kwop))))))

(defun tuareg-skip-blank-and-comments ()
  (if tuareg-use-syntax-ppss
      (forward-comment (point-max))
    (skip-syntax-forward " ")
    (while (and (not (eobp)) (tuareg-in-comment-p)
                (search-forward "*)" nil t))
      (skip-syntax-forward " "))))

(defun tuareg-skip-back-blank-and-comments ()
  (skip-syntax-backward " ")
  (while (save-excursion (tuareg-backward-char)
                         (and (> (point) (point-min)) (tuareg-in-comment-p)))
    (tuareg-backward-char)
    (tuareg-beginning-of-literal-or-comment) (skip-syntax-backward " ")))

(defun tuareg-find-phrase-beginning (&optional stop-at-and)
  "Find `real' phrase beginning and return point."
  (beginning-of-line)
  (tuareg-skip-blank-and-comments)
  (end-of-line)
  (tuareg-skip-to-end-of-phrase)
  (let ((old-point (point)) (pt (point)))
    (if stop-at-and
        (tuareg-find-kwop tuareg-find-phrase-beginning-and-regexp "and")
      (tuareg-find-kwop tuareg-find-phrase-beginning-regexp))
    (while (and (> (point) (point-min)) (< (point) old-point)
                (or (not (looking-at tuareg-find-phrase-beginning-and-regexp))
                    (and (looking-at "\\<let\\>")
                         (tuareg-looking-at-internal-let))
                    (and (looking-at "\\<and\\>")
                         (save-excursion
                           (tuareg-find-and-match)
                           (tuareg-looking-at-internal-let)))
                    (and (looking-at "\\<module\\>")
                         (tuareg-looking-at-false-module))
                    (and (looking-at tuareg-sig-struct-regexp)
                         (tuareg-looking-at-false-sig-struct))
                    (and (looking-at "\\<type\\>")
                         (tuareg-looking-at-false-type))))
      (when (= pt (point))
        (error "tuareg-find-phrase-beginning: inf loop at %d" pt))
      (setq pt (point))
      (if (looking-at "\\<end\\>")
          (tuareg-find-match)
        (unless (bolp) (tuareg-backward-char))
        (setq old-point (point))
        (if stop-at-and
            (tuareg-find-kwop tuareg-find-phrase-beginning-and-regexp "and")
          (tuareg-find-kwop tuareg-find-phrase-beginning-regexp))))
    (when (tuareg-at-phrase-break-p)
      (end-of-line) (tuareg-skip-blank-and-comments))
    (back-to-indentation)
    (point)))


(defun tuareg-search-forward-end ()
  (let ((begin (point)) (current -1) (found) (move t))
    (while (and move (> (point) current))
      (if (re-search-forward "\\<end\\>" (point-max) t)
          (let ((stop (point)) (kwop))
            (unless (tuareg-in-literal-or-comment-p)
              (save-excursion
                (tuareg-backward-char 3)
                (setq kwop (tuareg-find-match))
                (cond
                 ((string= kwop "object")
                  (tuareg-find-phrase-beginning))
                 ((and (looking-at tuareg-sig-struct-regexp)
                       (tuareg-looking-at-false-sig-struct))
                  (tuareg-find-phrase-beginning)))
                (cond
                 ((or
                   (> (point) begin)
                   (and
                    (string= kwop "sig")
                    (looking-at
                     "[ \t\n]*\\(\\<with\\>[ \t\n]*\\<type\\>\\|=\\)")))
                  (if (> (point) current)
                      (progn
                        (setq current (point))
                        (goto-char stop))
                    (setq found nil move nil)))
                 (t (setq found t move nil))))))
        (setq found nil move nil)))
    found))

(defun tuareg-inside-module-or-class-find-kwop ()
  (let ((kwop (tuareg-find-kwop tuareg-inside-module-or-class-regexp
                                "\\<\\(and\\|end\\)\\>")))
    (tuareg-reset-and-kwop kwop)
    (when (string= kwop "with") (setq kwop nil))
    (if (string= kwop "end")
        (progn
          (tuareg-find-match)
          (tuareg-find-kwop tuareg-inside-module-or-class-regexp)
          (tuareg-inside-module-or-class-find-kwop))
      kwop)))

(defun tuareg-inside-module-or-class-p ()
  (let ((begin) (end) (and-end) (and-iter t) (kwop t))
    (save-excursion
      (when (looking-at "\\<and\\>")
        (tuareg-find-and-match))
      (setq begin (point))
      (unless (or (and (looking-at "\\<class\\>")
                       (save-excursion
                         (re-search-forward "\\<object\\>"
                                            (point-max) t)
                         (tuareg-find-phrase-beginning)
                         (> (point) begin)))
                  (and (looking-at "\\<module\\>")
                       (save-excursion
                         (re-search-forward tuareg-sig-struct-regexp
                                            (point-max) t)
                         (tuareg-find-phrase-beginning)
                         (> (point) begin))))
        (unless (looking-at tuareg-inside-module-or-class-opening-full)
          (setq kwop (tuareg-inside-module-or-class-find-kwop)))
        (when kwop
          (setq begin (point))
          (when (tuareg-search-forward-end)
            (tuareg-backward-char 3)
            (when (looking-at "\\<end\\>")
              (tuareg-forward-char 3)
              (setq end (point))
              (setq and-end (point))
              (tuareg-skip-blank-and-comments)
              (while (and and-iter (looking-at "\\<and\\>"))
                (setq and-end (point))
                (when (tuareg-search-forward-end)
                  (tuareg-backward-char 3)
                  (when (looking-at "\\<end\\>")
                    (tuareg-forward-char 3)
                    (setq and-end (point))
                    (tuareg-skip-blank-and-comments)))
                (when (<= (point) and-end)
                  (setq and-iter nil)))
              (list begin end and-end))))))))

(defun tuareg-move-inside-module-or-class-opening ()
  "Go to the beginning of the enclosing module or class.

Notice that white-lines (or comments) located immediately before a
module/class are considered enclosed in this module/class."
  (interactive)
  (let* ((old-point (point))
         (kwop (tuareg-inside-module-or-class-find-kwop)))
    (unless kwop
      (goto-char old-point))
    (tuareg-find-phrase-beginning)))

(defun tuareg-discover-phrase (&optional quiet stop-at-and)
  (end-of-line)
  (let ((end (point)) (case-fold-search nil))
   (tuareg-with-internal-syntax
    (tuareg-find-phrase-beginning stop-at-and)
    (when (> (point) end) (setq end (point)))
    (save-excursion
      (let ((begin (point)) (cpt 0) (lines-left 0) (stop)
            (inside-module-or-class (tuareg-inside-module-or-class-p))
            (looking-block
             (looking-at tuareg-inside-module-or-class-opening-full)))
        (if (and looking-block inside-module-or-class)
            (progn
              (setq begin (nth 0 inside-module-or-class))
              (setq end (nth 2 inside-module-or-class))
              (goto-char end))
          (if inside-module-or-class
              (progn
                (setq stop (save-excursion
                             (goto-char (nth 1 inside-module-or-class))
                             (line-beginning-position)))
                (if (< stop end) (setq stop (point-max))))
            (setq stop (point-max)))
          (save-restriction
            (goto-char end)
            (while (and (= lines-left 0)
                        (or (not inside-module-or-class) (< (point) stop))
                        (<= (save-excursion
                              (tuareg-find-phrase-beginning stop-at-and)) end))
              (unless quiet
                (setq cpt (1+ cpt))
                (when (= 8 cpt)
                  (message "Looking for enclosing phrase...")))
              (setq end (point))
              (tuareg-skip-to-end-of-phrase)
              (narrow-to-region (line-beginning-position) (point-max))
              (goto-char end)
              (setq lines-left (forward-line 1)))))
        (when (>= cpt 8) (message "Looking for enclosing phrase... done."))
        (save-excursion (tuareg-skip-blank-and-comments) (setq end (point)))
        (tuareg-skip-back-blank-and-comments)
        (list begin (point) end))))))

(defun tuareg-mark-phrase ()
  "Put mark at end of this OCaml phrase, point at beginning.
The OCaml phrase is the phrase just before the point."
  (interactive)
  (let ((pair (tuareg-discover-phrase)))
    (goto-char (nth 1 pair)) (push-mark (nth 0 pair) t t)))

(defun tuareg-next-phrase (&optional quiet stop-at-and)
  "Skip to the beginning of the next phrase."
  (interactive "i")
  (goto-char (save-excursion
               (nth 2 (tuareg-discover-phrase quiet stop-at-and))))
  (cond
   ((looking-at "\\<end\\>")
    (tuareg-next-phrase quiet stop-at-and))
   ((looking-at ")")
    (forward-char 1)
    (tuareg-skip-blank-and-comments))
   ((looking-at ";;")
    (forward-char 2)
    (tuareg-skip-blank-and-comments))))

(defun tuareg-previous-phrase ()
  "Skip to the beginning of the previous phrase."
  (interactive)
  (beginning-of-line)
  (tuareg-skip-to-end-of-phrase)
  (tuareg-discover-phrase))

(defun tuareg-indent-phrase ()
  "Depending of the context: justify and indent a comment,
or indent all lines in the current phrase."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (if (tuareg-in-comment-p)
        (let* ((cobpoint (save-excursion
                           (tuareg-beginning-of-literal-or-comment)
                           (point)))
               (begpoint (save-excursion
                           (while (and (> (point) cobpoint)
                                       (tuareg-in-comment-p)
                                       (not (looking-at "^[ \t]*$")))
                             (forward-line -1))
                           (max cobpoint (point))))
               (coepoint (save-excursion
                           (while (tuareg-in-comment-p)
                             (re-search-forward "\\*)" nil 'end))
                           (point)))
               (endpoint (save-excursion
                           (re-search-forward "^[ \t]*$" coepoint 'end)
                           (line-beginning-position 2)))
               (leading-star (tuareg-leading-star-p)))
          (goto-char begpoint)
          (while (and leading-star
                      (< (point) endpoint)
                      (not (looking-at "^[ \t]*$")))
            (forward-line 1)
            (back-to-indentation)
            (when (looking-at "\\*\\**\\([^)]\\|$\\)")
              (delete-char 1)
              (setq endpoint (1- endpoint))))
          (goto-char (min (point) endpoint))
          (fill-region begpoint endpoint)
          (re-search-forward "\\*)" nil 'end)
          (setq endpoint (point))
          (when leading-star
            (goto-char begpoint)
            (forward-line 1)
            (if (< (point) endpoint)
                (tuareg-auto-fill-insert-leading-star t)))
          (indent-region begpoint endpoint nil))
      (let ((pair (tuareg-discover-phrase)))
        (indent-region (nth 0 pair) (nth 1 pair) nil)))))

(autoload 'ocaml-module-alist "caml-help")
(autoload 'ocaml-visible-modules "caml-help")
(autoload 'ocaml-module-symbols "caml-help")

(defun tuareg-completion-at-point ()
  (let ((beg (save-excursion (skip-syntax-backward "w_") (point)))
        (end (save-excursion (skip-syntax-forward "w_") (point)))
        (table
         (lambda (string pred action)
           (let ((dot (string-match "\\.[^.]*\\'" string))
                 ;; ocaml-module-symbols contains an unexplained call to
                 ;; pop-to-buffer within save-window-excursion.  Let's try and
                 ;; avoid it pops up a stupid frame.
                 (special-display-buffer-names
                  (cons '("*caml-help*" (same-frame . t))
                        special-display-buffer-names)))
             (if (eq (car-safe action) 'boundaries)
                 `(boundaries ,(if dot (1+ dot) 0)
                              ,@(string-match "\\." (cdr action)))
               (if (null dot)
                   (complete-with-action
                    action (apply #'append
                                  (mapcar (lambda (mod) (concat (car mod) "."))
                                          (ocaml-module-alist))
                                  (mapcar #'ocaml-module-symbols
                                          (ocaml-visible-modules)))
                    string pred)
                 (completion-table-with-context
                  (substring string 0 (1+ dot))
                  (ocaml-module-symbols
                   (assoc (substring string 0 dot) (ocaml-module-alist)))
                  (substring string (1+ dot)) pred action)))))))
    (unless (or (eq beg end)
                (not tuareg-with-caml-mode-p))
      (list beg end table))))


(autoload 'caml-complete "caml-help")

(defun tuareg-complete (arg)
  "Completes qualified ocaml identifiers."
  (interactive "p")
  (modify-syntax-entry ?_ "w" tuareg-mode-syntax-table)
  (unwind-protect
      (caml-complete arg)
    (modify-syntax-entry ?_ "_" tuareg-mode-syntax-table)))

(defun tuareg--try-find-alternate-file (mod-name extension &optional no-create)
  "Switch to the file given by MOD-NAME and EXTENSION.
If NO-CREATE is non-nil and the file doesn't exist, don't switch and return nil,
otherwise return non-nil."
  (let* ((filename (concat mod-name extension))
         (buffer (get-file-buffer filename))
         (what (cond
                ((string= extension ".ml") "implementation")
                ((string= extension ".mli") "interface"))))
    (cond
     (buffer (switch-to-buffer buffer))
     ((file-exists-p filename) (find-file filename))
     ((and (not no-create)
           (y-or-n-p
            (format "Create %s file %s " what
                    (file-name-nondirectory filename))))
      (find-file filename)))))

(defun tuareg-find-alternate-file ()
  "Switch Implementation/Interface."
  (interactive)
  (let ((name buffer-file-name))
    (when (string-match "\\`\\(.*\\)\\.ml\\([il]\\)?\\'" name)
      (let ((mod-name (tuareg-match-string 1 name))
	    (e (tuareg-match-string 2 name)))
	(cond
	 ((string= e "i")
	  (unless (tuareg--try-find-alternate-file mod-name ".mll" 'no-create)
	    (tuareg--try-find-alternate-file mod-name ".ml")))
	 (t
	  (tuareg--try-find-alternate-file mod-name ".mli")))))))

(define-skeleton tuareg-insert-class-form
  "Insert a nicely formatted class-end form, leaving a mark after end."
  nil
  \n "class " @ " = object (self)" > \n
  "inherit " > _ " as super" \n "end;;" > \n)

(define-skeleton tuareg-insert-begin-form
  "Insert a nicely formatted begin-end form, leaving a mark after end."
  nil
  \n "begin" > \n _ \n "end" > \n)

(define-skeleton tuareg-insert-for-form
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  nil
  \n "for " - " do" > \n _ \n "done" > \n)

(define-skeleton tuareg-insert-while-form
  "Insert a nicely formatted for-to-done form, leaving a mark after done."
  nil
  \n "while " - " do" > \n _ \n "done" > \n)

(define-skeleton tuareg-insert-if-form
  "Insert a nicely formatted if-then-else form, leaving a mark after else."
  nil
  \n "if" > \n _ \n "then" > \n @ \n "else" \n @)

(define-skeleton tuareg-insert-match-form
  "Insert a nicely formatted math-with form, leaving a mark after with."
  nil
  \n "match" > \n _ \n "with" > \n)

(define-skeleton tuareg-insert-let-form
  "Insert a nicely formatted let-in form, leaving a mark after in."
  nil
  \n "let " > _ " in" > \n)

(define-skeleton tuareg-insert-try-form
  "Insert a nicely formatted try-with form, leaving a mark after with."
  nil
  \n "try" > \n _ \n "with" > \n)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            Tuareg interactive mode

;; Augment Tuareg mode with a OCaml toplevel.

(require 'comint)

(defvar tuareg-interactive-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "|" 'tuareg-electric-pipe)
    (define-key map ")" 'tuareg-electric-rp)
    (define-key map "}" 'tuareg-electric-rc)
    (define-key map "]" 'tuareg-electric-rb)
    (define-key map "\C-c\C-i" 'tuareg-interrupt-ocaml)
    (define-key map "\C-c\C-k" 'tuareg-kill-ocaml)
    (define-key map "\C-c`" 'tuareg-interactive-next-error-toplevel)
    (define-key map "\C-c?" 'tuareg-interactive-next-error-toplevel)
    (define-key map "\C-m" 'tuareg-interactive-send-input)
    (define-key map "\C-j" 'tuareg-interactive-send-input-or-indent)
    (define-key map "\M-\C-m" 'tuareg-interactive-send-input-end-of-phrase)
    (define-key map [kp-enter] 'tuareg-interactive-send-input-end-of-phrase)
    map))

(defconst tuareg-interactive-buffer-name "*ocaml-toplevel*")

(defconst tuareg-interactive-toplevel-error-regexp
  "[ \t]*Characters \\([0-9]+\\)-\\([0-9]+\\):"
  "Regexp matching the char numbers in ocaml toplevel's error messages.")
(defvar tuareg-interactive-last-phrase-pos-in-source 0)
(defvar tuareg-interactive-last-phrase-pos-in-toplevel 0)

(defun tuareg-interactive-filter (text)
  (when (eq major-mode 'tuareg-interactive-mode)
    (save-excursion
      (when (>= comint-last-input-end comint-last-input-start)
        (when tuareg-interactive-read-only-input
          (add-text-properties
           comint-last-input-start comint-last-input-end
           (list 'read-only t)))
        (when (and font-lock-mode tuareg-interactive-input-font-lock)
          (font-lock-fontify-region comint-last-input-start
                                    comint-last-input-end))
        (when tuareg-interactive-output-font-lock
          (save-excursion
            (goto-char (point-max))
            (re-search-backward comint-prompt-regexp
                                comint-last-input-end t)
            (add-text-properties
             comint-last-input-end (point)
             '(face tuareg-font-lock-interactive-output-face))))
        (when tuareg-interactive-error-font-lock
          (save-excursion
            (goto-char comint-last-input-end)
            (while (re-search-forward tuareg-interactive-error-regexp () t)
              (let ((matchbeg (match-beginning 1))
                    (matchend (match-end 1)))
                (save-excursion
                  (goto-char matchbeg)
                  (put-text-property
                   matchbeg matchend
                   'face 'tuareg-font-lock-interactive-error-face)
                  (when (looking-at tuareg-interactive-toplevel-error-regexp)
                    (let ((beg (string-to-number (tuareg-match-string 1)))
                          (end (string-to-number (tuareg-match-string 2))))
                      (put-text-property
                       (+ comint-last-input-start beg)
                       (+ comint-last-input-start end)
                       'face 'tuareg-font-lock-error-face))))))))))))

(easy-menu-define
  tuareg-interactive-mode-menu tuareg-interactive-mode-map
  "Tuareg Interactive Mode Menu."
  '("Tuareg"
    ("Interactive Mode"
     ["Run OCaml Toplevel" tuareg-run-ocaml t]
     ["Interrupt OCaml Toplevel" tuareg-interrupt-ocaml
      :active (comint-check-proc tuareg-interactive-buffer-name)]
     ["Kill OCaml Toplevel" tuareg-kill-ocaml
      :active (comint-check-proc tuareg-interactive-buffer-name)]
     ["Evaluate Region" tuareg-eval-region :active (region-active-p)]
     ["Evaluate Phrase" tuareg-eval-phrase t]
     ["Evaluate Buffer" tuareg-eval-buffer t])
    "---"
    ["Customize Tuareg Mode..." (customize-group 'tuareg) t]
    ("Tuareg Options" ["Dummy" nil t])
    ("Tuareg Interactive Options" ["Dummy" nil t])
    "---"
    ["About" tuareg-about t]
    ["Help" tuareg-interactive-help t]))

(define-derived-mode tuareg-interactive-mode comint-mode "Tuareg-Interactive"
  "Major mode for interacting with an OCaml process.
Runs an OCaml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in tuareg mode.

Short cuts for interactions with the toplevel:
\\{tuareg-interactive-mode-map}"
  (add-hook 'comint-output-filter-functions 'tuareg-interactive-filter)
  (setq comint-prompt-regexp "^#  *")
  (setq comint-process-echoes nil)
  (setq comint-get-old-input 'tuareg-interactive-get-old-input)
  (setq comint-scroll-to-bottom-on-output
        tuareg-interactive-scroll-to-bottom-on-output)
  (set-syntax-table tuareg-mode-syntax-table)

  (tuareg--common-mode-setup)
  (when (or tuareg-interactive-input-font-lock
            tuareg-interactive-output-font-lock
            tuareg-interactive-error-font-lock)
    (font-lock-mode 1))
  (when (boundp 'after-change-functions) ;FIXME: Why?
    (remove-hook 'after-change-functions 'font-lock-after-change-function t))
  (when (boundp 'pre-idle-hook)
    (remove-hook 'pre-idle-hook 'font-lock-pre-idle-hook t))

  (easy-menu-add tuareg-interactive-mode-menu)
  (tuareg-update-options-menu))

;;;###autoload
(defun tuareg-run-ocaml ()
  "Run an OCaml toplevel process. I/O via buffer `*ocaml-toplevel*'."
  (interactive)
  (tuareg-run-process-if-needed)
  (display-buffer tuareg-interactive-buffer-name))

(defun tuareg-run-process-if-needed (&optional cmd)
  "Run an OCaml toplevel process if needed, with an optional command name.
I/O via buffer `*ocaml-toplevel*'."
  (if cmd
      (setq tuareg-interactive-program cmd)
    (unless (comint-check-proc tuareg-interactive-buffer-name)
      (setq tuareg-interactive-program
            (read-shell-command "OCaml toplevel to run: "
                                tuareg-interactive-program))))
  (unless (comint-check-proc tuareg-interactive-buffer-name)
    (let ((cmdlist (tuareg-args-to-list tuareg-interactive-program))
          (process-connection-type nil))
      (set-buffer (apply (function make-comint) "ocaml-toplevel"
                         (car cmdlist) nil (cdr cmdlist)))
      (tuareg-interactive-mode)
      (sleep-for 1))))

(defun tuareg-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((/= where 0)
           (cons (substring string 0 where)
                 (tuareg-args-to-list (substring string (+ 1 where)
                                                 (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (when pos
                 (tuareg-args-to-list (substring string pos
                                                 (length string)))))))))

(defun tuareg-interactive-get-old-input ()
  (save-excursion
    (let ((end (point)))
      (re-search-backward comint-prompt-regexp (point-min) t)
      (when (looking-at comint-prompt-regexp)
        (re-search-forward comint-prompt-regexp))
      (buffer-substring-no-properties (point) end))))

(defun tuareg-interactive-end-of-phrase ()
  (save-excursion
    (end-of-line)
    (tuareg-find-meaningful-word)
    (tuareg-find-meaningful-word)
    (looking-at ";;")))

(defun tuareg-interactive-send-input-end-of-phrase ()
  (interactive)
  (goto-char (point-max))
  (unless (tuareg-interactive-end-of-phrase)
    (insert ";;"))
  (comint-send-input))

(defconst tuareg-interactive-send-warning
  "Note: toplevel processing requires a terminating `;;'")

(defun tuareg-interactive-send-input ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline."
  (interactive)
  (if (tuareg-interactive-end-of-phrase)
      (progn
        (comint-send-input)
        (goto-char (point-max)))
    (insert "\n")
    (message tuareg-interactive-send-warning)))

(defun tuareg-interactive-send-input-or-indent ()
  "Process if the current line ends with `;;' then send the
current phrase else insert a newline and indent."
  (interactive)
  (if (tuareg-interactive-end-of-phrase)
      (progn
        (goto-char (point-max))
        (comint-send-input))
    (insert "\n")
    (indent-according-to-mode)
    (message tuareg-interactive-send-warning)))

(defun tuareg-eval-region (start end)
  "Eval the current region in the OCaml toplevel."
  (interactive "r")
  (save-excursion (tuareg-run-process-if-needed))
  (comint-preinput-scroll-to-bottom)
  (setq tuareg-interactive-last-phrase-pos-in-source start)
  (save-excursion
    (goto-char start)
    (tuareg-skip-blank-and-comments)
    (setq start (point))
    (goto-char end)
    (tuareg-skip-to-end-of-phrase)
    (setq end (point))
    (let ((text (buffer-substring-no-properties start end)))
      (goto-char end)
      (if (string= text "")
          (message "Cannot send empty commands to OCaml toplevel!")
        (set-buffer tuareg-interactive-buffer-name)
        (goto-char (point-max))
        (setq tuareg-interactive-last-phrase-pos-in-toplevel (point))
        (comint-send-string tuareg-interactive-buffer-name
                            (concat text ";;"))
        (let ((pos (point)))
          (comint-send-input)
          (when tuareg-interactive-echo-phrase
            (save-excursion
              (goto-char pos)
              (insert (concat text ";;")))))))
    (when tuareg-display-buffer-on-eval
      (display-buffer tuareg-interactive-buffer-name))))

(defun tuareg-narrow-to-phrase ()
  "Narrow the editting window to the surrounding OCaml phrase (or block)."
  (interactive)
  (save-excursion
    (let ((pair (tuareg-discover-phrase)))
      (narrow-to-region (nth 0 pair) (nth 1 pair)))))

(defun tuareg-eval-phrase ()
  "Eval the surrounding OCaml phrase (or block) in the Caml toplevel."
  (interactive)
  (let ((end))
    (save-excursion
      (let ((pair (tuareg-discover-phrase)))
        (setq end (nth 2 pair))
        (tuareg-eval-region (nth 0 pair) (nth 1 pair))))
    (when tuareg-skip-after-eval-phrase
      (goto-char end))))

(defun tuareg-eval-buffer ()
  "Send the buffer to the Tuareg Interactive process."
  (interactive)
  (tuareg-eval-region (point-min) (point-max)))

(defun tuareg-interactive-next-error-source ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (with-current-buffer tuareg-interactive-buffer-name
      (goto-char tuareg-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
            (re-search-forward tuareg-interactive-toplevel-error-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (tuareg-match-string 1))
              end (string-to-number (tuareg-match-string 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ tuareg-interactive-last-phrase-pos-in-source beg)
            end (+ tuareg-interactive-last-phrase-pos-in-source end))
      (goto-char beg)
      (put-text-property beg end 'face 'tuareg-font-lock-error-face))))

(defun tuareg-interactive-next-error-toplevel ()
  (interactive)
  (let ((error-pos) (beg 0) (end 0))
    (save-excursion
      (goto-char tuareg-interactive-last-phrase-pos-in-toplevel)
      (setq error-pos
            (re-search-forward tuareg-interactive-toplevel-error-regexp
                               (point-max) t))
      (when error-pos
        (setq beg (string-to-number (tuareg-match-string 1))
              end (string-to-number (tuareg-match-string 2)))))
    (if (not error-pos)
        (message "No syntax or typing error in last phrase.")
      (setq beg (+ tuareg-interactive-last-phrase-pos-in-toplevel beg)
            end (+ tuareg-interactive-last-phrase-pos-in-toplevel end))
      (put-text-property beg end 'face 'tuareg-font-lock-error-face)
      (goto-char beg))))

(defun tuareg-interrupt-ocaml ()
  (interactive)
  (when (comint-check-proc tuareg-interactive-buffer-name)
    (with-current-buffer tuareg-interactive-buffer-name
      (comint-interrupt-subjob))))

(defun tuareg-kill-ocaml ()
  (interactive)
  (when (comint-check-proc tuareg-interactive-buffer-name)
    (with-current-buffer tuareg-interactive-buffer-name
      (comint-kill-subjob))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Menu support

(defun tuareg-about ()
  (interactive)
  (describe-variable 'tuareg-mode-version))

(defun tuareg-short-cuts ()
  "Short cuts for the Tuareg mode:
\\{tuareg-mode-map}

Short cuts for interaction within the toplevel:
\\{tuareg-interactive-mode-map}"
  (interactive)
  (describe-function 'tuareg-short-cuts))

(defun tuareg-help ()
  (interactive)
  (describe-function 'tuareg-mode))

(defun tuareg-interactive-help ()
  (interactive)
  (describe-function 'tuareg-interactive-mode))

(defvar tuareg-definitions-menu (list ["Scan..." tuareg-list-definitions t])
  "Initial content of the definitions menu.")
(make-variable-buffer-local 'tuareg-definitions-menu)

(defvar tuareg-definitions-menu-last-buffer nil)
(defvar tuareg-definitions-keymaps nil)

(defun tuareg-build-menu ()
  (easy-menu-define
   tuareg-mode-menu (list tuareg-mode-map)
   "Tuareg Mode Menu."
   '("Tuareg"
     ("Interactive Mode"
      ["Run OCaml Toplevel" tuareg-run-ocaml t]
      ["Interrupt OCaml Toplevel" tuareg-interrupt-ocaml
       :active (comint-check-proc tuareg-interactive-buffer-name)]
      ["Kill OCaml Toplevel" tuareg-kill-ocaml
       :active (comint-check-proc tuareg-interactive-buffer-name)]
      ["Evaluate Region" tuareg-eval-region
       ;; Region-active-p for XEmacs and mark-active for Emacs
       :active mark-active]
      ["Evaluate Phrase" tuareg-eval-phrase t]
      ["Evaluate Buffer" tuareg-eval-buffer t])
     ("OCaml Forms"
      ["try .. with .." tuareg-insert-try-form t]
      ["match .. with .." tuareg-insert-match-form t]
      ["let .. in .." tuareg-insert-let-form t]
      ["if .. then .. else .." tuareg-insert-if-form t]
      ["while .. do .. done" tuareg-insert-while-form t]
      ["for .. do .. done" tuareg-insert-for-form t]
      ["begin .. end" tuareg-insert-begin-form t])
     ["Switch .ml/.mli" tuareg-find-alternate-file t]
     "---"
     ["Compile..." compile t]
     ["Reference Manual..." tuareg-browse-manual t]
     ["OCaml Library..." tuareg-browse-library t]
     ("Definitions"
      ["Scan..." tuareg-list-definitions t])
     "---"
     [ "Show type at point" caml-types-show-type
       tuareg-with-caml-mode-p]
     [ "Show fully qualified ident at point" caml-types-show-ident
       tuareg-with-caml-mode-p]
     "---"
     [ "Complete identifier" caml-complete
       tuareg-with-caml-mode-p]
     [ "Help for identifier" caml-help
       tuareg-with-caml-mode-p]
     [ "Add path for documentation" ocaml-add-path
       tuareg-with-caml-mode-p]
     [ "Open module for documentation" ocaml-open-module
       tuareg-with-caml-mode-p]
     [ "Close module for documentation" ocaml-close-module
       tuareg-with-caml-mode-p]
     "---"
     ["Customize Tuareg Mode..." (customize-group 'tuareg) t]
     ("Tuareg Options" ["Dummy" nil t])
     ("Tuareg Interactive Options" ["Dummy" nil t])
     "---"
     ["About" tuareg-about t]
     ["Short Cuts" tuareg-short-cuts]
     ["Help" tuareg-help t]))
  (easy-menu-add tuareg-mode-menu)
  (tuareg-update-options-menu))

(defun tuareg-update-definitions-menu ()
  (when (eq major-mode 'tuareg-mode)
    (easy-menu-change
     '("Tuareg") "Definitions"
     tuareg-definitions-menu)))

(defun tuareg-toggle-option (symbol)
  (interactive)
  (set symbol (not (symbol-value symbol)))
  (when (eq 'tuareg-use-abbrev-mode symbol)
    (abbrev-mode tuareg-use-abbrev-mode)) ; toggle abbrev minor mode
  (tuareg-update-options-menu))

(defun tuareg-update-options-menu ()
  (easy-menu-change
   '("Tuareg") "Tuareg Options"
   (mapcar (lambda (pair)
             (if (consp pair)
                 (vector (car pair)
                         (list 'tuareg-toggle-option (cdr pair))
                         ':style 'toggle
                         ':selected (nth 1 (cdr pair))
                         ':active t)
               pair)) tuareg-options-list))
  (easy-menu-change
   '("Tuareg") "Tuareg Interactive Options"
   (mapcar (lambda (pair)
             (if (consp pair)
                 (vector (car pair)
                         (list 'tuareg-toggle-option (cdr pair))
                         ':style 'toggle
                         ':selected (nth 1 (cdr pair))
                         ':active t)
               pair)) tuareg-interactive-options-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Manual

;; From M. Quercia

(defun tuareg-browse-manual ()
  "*Browse OCaml reference manual."
  (interactive)
  (setq tuareg-manual-url (read-from-minibuffer "URL: " tuareg-manual-url))
  (funcall tuareg-browser tuareg-manual-url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Browse Library

;; From M. Quercia

(defvar tuareg-library-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [return] 'tuareg-library-find-file)
    (define-key map [mouse-2] 'tuareg-library-mouse-find-file)
    map))

(defun tuareg-browse-library()
  "Browse the OCaml library."
  (interactive)
  (let ((buf-name "*ocaml-library*") (opoint)
        (dir (read-from-minibuffer "Library path: " tuareg-library-path)))
    (when (and (file-directory-p dir) (file-readable-p dir))
      (setq tuareg-library-path dir)
      ;; List *.ml and *.mli files
      (with-output-to-temp-buffer buf-name
        (buffer-disable-undo standard-output)
        (with-current-buffer buf-name
          (kill-all-local-variables)
          (set (make-local-variable 'tuareg-library-path) dir)
          ;; Help
          (insert "Directory \"" dir "\".\n")
          (insert "Select a file with middle mouse button or RETURN.\n\n")
          (insert "Interface files (.mli):\n\n")
          (insert-directory (concat dir "/*.mli") "-C" t nil)
          (insert "\n\nImplementation files (.ml):\n\n")
          (insert-directory (concat dir "/*.ml") "-C" t nil)
          ;; '.', '-' and '_' are now letters
          (modify-syntax-entry ?. "w")
          (modify-syntax-entry ?_ "w")
          (modify-syntax-entry ?- "w")
          ;; Every file name is now mouse-sensitive
          (goto-char (point-min))
          (while (< (point) (point-max))
            (re-search-forward "\\.ml.?\\>")
            (setq opoint (point))
            (re-search-backward "\\<" (point-min) 1)
            (put-text-property (point) opoint 'mouse-face 'highlight)
            (goto-char (+ 1 opoint)))
          ;; Activate tuareg-library mode
          (setq major-mode 'tuareg-library-mode)
          (setq mode-name "tuareg-library")
          (use-local-map tuareg-library-mode-map)
          (setq buffer-read-only t))))))

(defun tuareg-library-find-file ()
  "Load the file whose name is near point."
  (interactive)
  (when (text-properties-at (point))    ;FIXME: Why??
    (save-excursion
      (let (beg)
        (re-search-backward "\\<") (setq beg (point))
        (re-search-forward "\\>")
        (find-file-read-only (expand-file-name (buffer-substring-no-properties
                                                beg (point))
                                               tuareg-library-path))))))

(defun tuareg-library-mouse-find-file (event)
  "Visit the file name you click on."
  (interactive "e")
  (let ((owindow (selected-window)))
    (mouse-set-point event)
    (tuareg-library-find-file)
    (select-window owindow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Definitions List

;; Designed from original code by M. Quercia

(defconst tuareg--id-regexp "[[:alpha:]][_'[:alnum:]]*")

(defconst tuareg-definitions-bind-skip-regexp
  (concat (tuareg-ro "rec" "type" "virtual") "\\|'"
          tuareg--id-regexp "\\|('.*)")
  "Regexp matching stuff to ignore after a binding keyword.")

(defconst tuareg-identifier-regexp (concat "\\<" tuareg--id-regexp "\\>"))

(defun tuareg-imenu-create-index ()
  (let ((cpt (if (fboundp 'make-progress-reporter)
                 (make-progress-reporter "Searching definitions..."
                                         (point-min) (point-max))
               0))
        (kw) (value-list) (type-list) (module-list) (class-list) (misc-list))
    (goto-char (point-min))
    (tuareg-skip-blank-and-comments)
    (while (not (eobp))
      (when (looking-at tuareg-definitions-regexp)
        (setq kw (tuareg-match-string 0))
        (save-match-data (tuareg-reset-and-kwop kw))
        (when (member kw '("exception" "val"))
          (setq kw "let"))
        ;; Skip optional elements
        (goto-char (match-end 0))
        (tuareg-skip-blank-and-comments)
        (when (looking-at tuareg-definitions-bind-skip-regexp)
          (goto-char (match-end 0)))
        (tuareg-skip-blank-and-comments)
        (when (looking-at tuareg-identifier-regexp)
          (let ((ref (cons (tuareg-match-string 0) (point-marker))))
            (if (not (integerp cpt))
                (progress-reporter-update cpt (point))
              (setq cpt (1+ cpt))
              (message "Searching definitions... (%d)" cpt))
            (cond ((string= kw "let")
                   (setq value-list (cons ref value-list)))
                  ((string= kw "type")
                   (setq type-list (cons ref type-list)))
                  ((string= kw "module")
                   (setq module-list (cons ref module-list)))
                  ((string= kw "class")
                   (setq class-list (cons ref class-list)))
                  (t (setq misc-list (cons ref misc-list)))))))
      ;; Skip to next phrase or next top-level `and'
      (tuareg-forward-char)
      (let ((old-point (point))
            (last-and (progn (tuareg-next-phrase t t) (point))))
        (when (< last-and old-point) (error "Scan error"))
        (save-excursion
          (while (and (re-search-backward "\\<and\\>" old-point t)
                      (not (tuareg-in-literal-or-comment-p))
                      (save-excursion (tuareg-find-and-match)
                                      (>= old-point (point))))
            (setq last-and (point))))
        (goto-char last-and)))
    (if (integerp cpt)
        (message "Searching definitions... done")
      (progress-reporter-done cpt))
    (let ((index ()))
      (when module-list (push (cons "Modules" module-list) index))
      (when type-list   (push (cons "Types" type-list) index))
      (when class-list  (push (cons "Classes" class-list) index))
      (when value-list  (push (cons "Values" value-list) index))
      (when misc-list   (push (cons "Miscellaneous" misc-list) index))
      index)))

(defun tuareg-list-definitions ()
  "Parse the buffer and gather toplevel definitions
for a quick jump via the definitions menu."
  (interactive)
  (let ((defs (save-excursion (tuareg-imenu-create-index)))
        menu)
    ;; Sort and build lists
    (dolist (pair defs)
      (let ((entries (mapcar (lambda (elem)
                               (vector (car elem)
                                       (list 'tuareg-goto (cdr elem))
                                       t))
                             (cdr pair))))
        (setq menu
              (append (tuareg-split-long-list
                       (car pair) (tuareg-sort-definitions entries))
                      menu))))
    ;; Update definitions menu
    (setq tuareg-definitions-menu
          (append menu (list "---"
                             ["Rescan..." tuareg-list-definitions t]))))
  (tuareg-update-definitions-menu))

(defun tuareg-goto (pos)
  (goto-char pos)
  (recenter))

(defun tuareg-sort-definitions (list)
  (let* ((last "") (cpt 1)
         (list (sort (nreverse list)    ;FIXME: Why reverse before sorting?
                     (lambda (p q) (string< (elt p 0) (elt q 0)))))
         (tail list))
    (while tail
      (if (string= (elt (car tail) 0) last)
          (progn
            (setq cpt (1+ cpt))
            (aset (car tail) 0 (format "%s (%d)" last cpt)))
        (setq cpt 1)
        (setq last (elt (car tail) 0)))
      (setq tail (cdr tail)))
    list))

;; Split a definition list if it is too long
(defun tuareg-split-long-list (title list)
  (let ((tail (nthcdr tuareg-definitions-max-items list)))
    (if (null (cdr tail))
        ;; List not too long, cons the title
        (list (cons title list))
      ;; List too long, split and add initials to the title
      (let (lists)
        (while list
          (let ((beg (substring (elt (car list) 0) 0 1))
                (end (substring (elt (car tail) 0) 0 1)))
            (setq lists (cons
                         (cons (format "%s %s-%s" title beg end) list)
                         lists))
            (setq list (cdr tail))
            (setcdr tail nil)
            (setq tail (nthcdr tuareg-definitions-max-items list))))
        (nreverse lists)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             Hooks and Exit

(eval-when-compile
  (autoload 'speedbar-add-supported-extension "speedbar"))
(when (require 'speedbar nil t)
  (speedbar-add-supported-extension
   '(".ml" ".mli" ".mll" ".mly" ".ls")))

(defvar tuareg-load-hook nil
  "This hook is run when Tuareg is loaded in. It is a good place to put
key-bindings or hack Font-Lock keywords...")

(run-hooks 'tuareg-load-hook)

(provide 'tuareg)
;; For compatibility with caml support modes
;; you may also link caml.el to tuareg.el
(provide 'caml)

;;; tuareg.el ends here
