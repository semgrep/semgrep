;; caml-font: font-lock support for OCaml files
;;
;; rewrite and clean-up.
;; Changes:
;; - fontify strings and comments using syntactic font lock
;; - define a `font-lock-syntactic-face-function' to fontify ocamldoc comments
;; - fontify infix operators like mod, land, lsl, etc.
;; - fontify line number directives
;; - fontify "failwith" and "invalid_arg" like "raise"
;; - fontify '\x..' character constants
;; - use the regexp-opt function to build regexps (more readable)
;; - use backquote and comma in sexp (more readable)
;; - drop the `caml-quote-char' variable (I don't use caml-light :))
;; - stop doing weird things with faces


(require 'font-lock)

(defvar caml-font-stop-face
  (progn
    (make-face 'caml-font-stop-face)
    (set-face-foreground 'caml-font-stop-face "White")
    (set-face-background 'caml-font-stop-face "Red")
    'caml-font-stop-face))

(defvar caml-font-doccomment-face
  (progn
    (make-face 'caml-font-doccomment-face)
    (set-face-foreground 'caml-font-doccomment-face "Red")
    'caml-font-doccomment-face))

(unless (facep 'font-lock-preprocessor-face)
  (defvar font-lock-preprocessor-face
    (copy-face 'font-lock-builtin-face
               'font-lock-preprocessor-face)))

(defconst caml-font-lock-keywords
  `(
;character literals
    ("'\\(.\\|\\\\\\([ntbr\"'\\\\]\\|[0-9]\\{3\\}\\|x[0-9A-Fa-f]\\{2\\}\\)\\)'"
     . font-lock-string-face)
;modules and constructors
   ("`?\\<[A-Z][A-Za-z0-9_']*\\>" . font-lock-function-name-face)
;definition
   (,(regexp-opt '("and" "as" "constraint" "class"
                   "exception" "external" "fun" "function" "functor"
                   "in" "inherit" "initializer" "let"
                   "method" "mutable" "module" "of" "private" "rec"
                   "type" "val" "virtual")
                 'words)
    . font-lock-type-face)
;blocking
   (,(regexp-opt '("begin" "end" "object" "sig" "struct") 'words)
    . font-lock-keyword-face)
;linenums
   ("# *[0-9]+" . font-lock-preprocessor-face)
;infix operators
   (,(regexp-opt '("asr" "land" "lor" "lsl" "lsr" "lxor" "mod") 'words)
    . font-lock-builtin-face)
;control
   (,(concat "[|#&]\\|->\\|"
             (regexp-opt '("do" "done" "downto" "else" "for" "if" "ignore"
                           "lazy" "match" "new" "or" "then" "to" "try"
                           "when" "while" "with")
                         'words))
    . font-lock-constant-face)
   ("\\<raise\\|failwith\\|invalid_arg\\>"
    . font-lock-comment-face)
;labels (and open)
   ("\\(\\([~?]\\|\\<\\)[a-z][a-zA-Z0-9_']*:\\)[^:=]"
    1 font-lock-variable-name-face)
   ("\\<\\(assert\\|open\\|include\\)\\>\\|[~?][ (]*[a-z][a-zA-Z0-9_']*"
    . font-lock-variable-name-face)))


(defun caml-font-syntactic-face (s)
  (let ((in-string  (nth 3 s))
        (in-comment (nth 4 s))
        (start      (nth 8 s)))
    (cond
     (in-string 'font-lock-string-face)
     (in-comment
      (save-excursion
        (goto-char start)
        (cond
         ((looking-at "(\\*\\*/\\*\\*)") 'caml-font-stop-face)
         ((looking-at "(\\*\\*[^*]")     'caml-font-doccomment-face)
         (t                              'font-lock-comment-face)))))))


;; font-lock commands are similar for caml-mode and inferior-caml-mode
(defun caml-font-set-font-lock ()
  (setq font-lock-defaults
        '(caml-font-lock-keywords
          nil nil nil nil
          (font-lock-syntactic-face-function . caml-font-syntactic-face)))
  (font-lock-mode 1))
(add-hook 'caml-mode-hook 'caml-font-set-font-lock)



(defconst inferior-caml-font-lock-keywords
  `(("^[#-]" . font-lock-comment-face)
    ,@caml-font-lock-keywords))

(defun inferior-caml-set-font-lock ()
  (setq font-lock-defaults
        '(inferior-caml-font-lock-keywords
          nil nil nil nil
          (font-lock-syntactic-face-function . caml-font-syntactic-face)))
  (font-lock-mode 1))
(add-hook 'inferior-caml-mode-hooks 'inferior-caml-set-font-lock)

(provide 'caml-font)
