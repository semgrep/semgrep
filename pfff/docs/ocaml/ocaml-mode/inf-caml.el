;(***********************************************************************)
;(*                                                                     *)
;(*                           Objective Caml                            *)
;(*                                                                     *)
;(*                   Xavier Leroy and Jacques Garrigue                 *)
;(*                                                                     *)
;(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the GNU General Public License.                 *)
;(*                                                                     *)
;(***********************************************************************)

;(* $Id: inf-caml.el 11027 2011-05-05 11:28:57Z doligez $ *)

;;; inf-caml.el --- run the Caml toplevel in an Emacs buffer

;; Xavier Leroy, july 1993.

;; modified by Jacques Garrigue, july 1997.

(require 'comint)
(require 'caml)

;; User modifiable variables

;; Whether you want the output buffer to be diplayed when you send a phrase

(defvar caml-display-when-eval t
  "*If true, display the inferior caml buffer when evaluating expressions.")


;; End of User modifiable variables


(defvar inferior-caml-mode-map nil)
(if inferior-caml-mode-map nil
  (setq inferior-caml-mode-map
        (copy-keymap comint-mode-map)))

;; Augment Caml mode, so you can process Caml code in the source files.

(defvar inferior-caml-program "ocaml"
  "*Program name for invoking an inferior Caml from Emacs.")

(defun inferior-caml-mode ()
  "Major mode for interacting with an inferior Caml process.
Runs a Caml toplevel as a subprocess of Emacs, with I/O through an
Emacs buffer. A history of input phrases is maintained. Phrases can
be sent from another buffer in Caml mode.

\\{inferior-caml-mode-map}"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp "^# ?")
  (setq major-mode 'inferior-caml-mode)
  (setq mode-name "Inferior Caml")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(*")
  (make-local-variable 'comment-end)
  (setq comment-end "*)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (use-local-map inferior-caml-mode-map)
  (run-hooks 'inferior-caml-mode-hooks))


(defconst inferior-caml-buffer-subname "inferior-caml")
(defconst inferior-caml-buffer-name
  (concat "*" inferior-caml-buffer-subname "*"))

;; for compatibility with xemacs

(defun caml-sit-for (second &optional mili redisplay)
   (if (and (boundp 'running-xemacs) running-xemacs)
       (sit-for (if mili (+ second (* mili 0.001)) second) redisplay)
     (sit-for second mili redisplay)))

;; To show result of evaluation at toplevel

(defvar inferior-caml-output nil)
(defun inferior-caml-signal-output (s)
  (if (string-match "[^ ]" s) (setq inferior-caml-output t)))

(defun inferior-caml-mode-output-hook ()
  (set-variable 'comint-output-filter-functions
        (list (function inferior-caml-signal-output)) 
        t))
(add-hook 'inferior-caml-mode-hooks 'inferior-caml-mode-output-hook)

;; To launch ocaml whenever needed

(defun caml-run-process-if-needed (&optional cmd)
  (if (comint-check-proc inferior-caml-buffer-name) nil
    (if (not cmd)
        (if (comint-check-proc inferior-caml-buffer-name)
            (setq cmd inferior-caml-program)
          (setq cmd (read-from-minibuffer "Caml toplevel to run: "
                                          inferior-caml-program))))
    (setq inferior-caml-program cmd)
    (let ((cmdlist (inferior-caml-args-to-list cmd))
          (process-connection-type nil))
      (set-buffer (apply (function make-comint)
                         inferior-caml-buffer-subname
                         (car cmdlist) nil (cdr cmdlist)))
      (inferior-caml-mode)
      (display-buffer inferior-caml-buffer-name)
      t)
    (setq caml-shell-active t)
    ))

;; patched to from original run-caml sharing code with
;;  caml-run-process-when-needed

(defun run-caml (&optional cmd)
  "Run an inferior Caml process.
Input and output via buffer `*inferior-caml*'."
  (interactive
   (list (if (not (comint-check-proc inferior-caml-buffer-name))
             (read-from-minibuffer "Caml toplevel to run: "
                                   inferior-caml-program))))
  (caml-run-process-if-needed cmd)
  (switch-to-buffer-other-window inferior-caml-buffer-name))


(defun inferior-caml-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (inferior-caml-args-to-list (substring string (+ 1 where)
                                                        (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                 (inferior-caml-args-to-list (substring string pos
                                                        (length string)))))))))

(defun inferior-caml-show-subshell ()
  (interactive)
  (caml-run-process-if-needed)
  (display-buffer inferior-caml-buffer-name)
  ; Added by Didier to move the point of inferior-caml to end of buffer
  (let ((buf (current-buffer))
        (caml-buf  (get-buffer inferior-caml-buffer-name))
        (count 0))
    (while
        (and (< count 10)
             (not (equal (buffer-name (current-buffer))
                         inferior-caml-buffer-name)))
      (next-multiframe-window)
      (setq count (+ count 1)))
    (if  (equal (buffer-name (current-buffer))
                inferior-caml-buffer-name)
        (goto-char (point-max)))
    (while
        (> count 0)
      (previous-multiframe-window)
      (setq count (- count 1)))
    )
)

;; patched by Didier to move cursor after evaluation

(defun inferior-caml-eval-region (start end)
  "Send the current region to the inferior Caml process."
  (interactive "r")
  (save-excursion (caml-run-process-if-needed))
  (save-excursion
    (goto-char end)
    (caml-skip-comments-backward)
    (comint-send-region inferior-caml-buffer-name start (point))
    ;; normally, ";;" are part of the region
    (if (and (>= (point) 2)
             (prog2 (backward-char 2) (looking-at ";;")))
        (comint-send-string inferior-caml-buffer-name "\n")
      (comint-send-string inferior-caml-buffer-name ";;\n"))
    ;; the user may not want to see the output buffer
    (if caml-display-when-eval
        (display-buffer inferior-caml-buffer-name t))))

;; jump to errors produced by ocaml compiler

(defun inferior-caml-goto-error (start end)
  "Jump to the location of the last error as indicated by inferior toplevel."
  (interactive "r")
  (let ((loc (+ start
                (save-excursion
                  (set-buffer (get-buffer inferior-caml-buffer-name))
                  (re-search-backward
                   (concat comint-prompt-regexp
                           "[ \t]*Characters[ \t]+\\([0-9]+\\)-[0-9]+:$"))
                  (caml-string-to-int (match-string 1))))))
    (goto-char loc)))


;;; orgininal inf-caml.el ended here

;; as eval-phrase, but ignores errors.

(defun inferior-caml-just-eval-phrase (arg &optional min max)
  "Send the phrase containing the point to the CAML process.
With prefix-arg send as many phrases as its numeric value,
ignoring possible errors during evaluation.

Optional arguments min max defines a region within which the phrase
should lies."
  (interactive "p")
  (let ((beg))
    (while (> arg 0)
      (setq arg (- arg 1))
      (setq beg  (caml-find-phrase min max))
      (caml-eval-region beg (point)))
    beg))

(defvar caml-previous-output nil
  "tells the beginning of output in the shell-output buffer, so that the
output can be retreived later, asynchronously.")

;; enriched version of eval-phrase, to repport errors.

(defun inferior-caml-eval-phrase (arg &optional min max)
  "Send the phrase containing the point to the CAML process.
With prefix-arg send as many phrases as its numeric value,
If an error occurs during evalutaion, stop at this phrase and
repport the error.

Return nil if noerror and position of error if any.

If arg's numeric value is zero or negative, evaluate the current phrase
or as many as prefix arg, ignoring evaluation errors.
This allows to jump other erroneous phrases.

Optional arguments min max defines a region within which the phrase
should lies."
  (interactive "p")
  (if (save-excursion (caml-run-process-if-needed))
      (progn
        (setq inferior-caml-output nil)
        (caml-wait-output 10 1)))
  (if (< arg 1) (inferior-caml-just-eval-phrase (max 1 (- 0 arg)) min max)
    (let ((proc (get-buffer-process inferior-caml-buffer-name))
          (buf (current-buffer))
          previous-output orig beg end err)
      (save-window-excursion
        (while (and (> arg 0) (not err))
          (setq previous-output (marker-position (process-mark proc)))
          (setq caml-previous-output previous-output)
          (setq inferior-caml-output nil)
          (setq orig (inferior-caml-just-eval-phrase 1 min max))
          (caml-wait-output)
          (switch-to-buffer inferior-caml-buffer-name  nil)
          (goto-char previous-output)
          (cond ((re-search-forward
                  " *Characters \\([01-9][01-9]*\\)-\\([1-9][01-9]*\\):\n[^W]"
                  (point-max) t)
                 (setq beg (caml-string-to-int (caml-match-string 1)))
                 (setq end (caml-string-to-int (caml-match-string 2)))
                 (switch-to-buffer buf)
                 (goto-char orig)
                 (forward-byte end)
                 (setq end (point))
                 (goto-char orig)
                 (forward-byte beg)
                 (setq beg (point))
                 (setq err beg)
                 )
                ((looking-at
                  "Toplevel input:\n[>]\\([^\n]*\\)\n[>]\\(\\( *\\)^*\\)\n")
                 (let ((expr (caml-match-string 1))
                       (column (-   (match-end 3) (match-beginning 3)))
                       (width (-   (match-end 2) (match-end 3))))
                   (if (string-match  "^\\(.*\\)[<]EOF[>]$" expr)
                       (setq expr (substring expr (match-beginning 1) (match-end 1))))
                   (switch-to-buffer buf)
                   (re-search-backward
                    (concat "^" (regexp-quote expr) "$")
                    (- orig 10))
                   (goto-char (+ (match-beginning 0) column))
                   (setq end (+ (point) width)))
                 (setq err beg))
                ((looking-at
                  "Toplevel input:\n>[.]*\\([^.].*\n\\)\\([>].*\n\\)*[>]\\(.*[^.]\\)[.]*\n")
                 (let* ((e1 (caml-match-string 1))
                        (e2 (caml-match-string 3))
                        (expr
                         (concat
                          (regexp-quote e1) "\\(.*\n\\)*" (regexp-quote e2))))
                   (switch-to-buffer buf)
                   (re-search-backward expr orig 'move)
                   (setq end (match-end 0)))
                 (setq err beg))
                (t
                 (switch-to-buffer buf)))
          (setq arg (- arg 1))
          )
        (pop-to-buffer inferior-caml-buffer-name)
        (if err
            (goto-char (point-max))
          (goto-char previous-output)
          (goto-char (point-max)))
        (pop-to-buffer buf))
      (if err (progn (beep) (caml-overlay-region (point) end))
        (if inferior-caml-output
            (message "No error")
          (message "No output yet...")
          ))
      err)))

(defun caml-overlay-region (beg end &optional wait)
  (interactive "%r")
  (cond ((fboundp 'make-overlay)
         (if caml-error-overlay ()
           (setq caml-error-overlay (make-overlay 1 1))
           (overlay-put caml-error-overlay 'face 'region))
         (unwind-protect
             (progn
               (move-overlay caml-error-overlay beg end (current-buffer))
               (beep) (if wait (read-event) (caml-sit-for 60)))
           (delete-overlay caml-error-overlay)))))

;; wait some amount for output, that is, until inferior-caml-output is set
;; to true. Hence, interleaves sitting for shorts delays and checking the
;; flag. Give up after some time. Typing into the source buffer will cancel
;; waiting, i.e. may report 'No result yet'

(defun caml-wait-output (&optional before after)
  (let ((c 1))
    (caml-sit-for 0 (or before 1))
    (let ((c 1))
      (while (and (not inferior-caml-output) (< c 99) (caml-sit-for 0 c t))
        (setq c (+ c 1))))
    (caml-sit-for (or after 0) 1)))

;; To insert the last output from caml at point
(defun caml-insert-last-output ()
  "Insert the result of the evaluation of previous phrase"
  (interactive)
  (let ((pos (process-mark (get-buffer-process inferior-caml-buffer-name))))
  (insert-buffer-substring inferior-caml-buffer-name
                           caml-previous-output (- pos 2))))

;; additional bindings

;(let ((map (lookup-key caml-mode-map [menu-bar caml])))
;  (define-key map [indent-buffer] '("Indent buffer" . caml-indent-buffer))
;  (define-key map [eval-buffer] '("Eval buffer" . caml-eval-buffer))
;)
;(define-key caml-mode-map "\C-c\C-b" 'caml-eval-buffer)


(provide 'inf-caml)
