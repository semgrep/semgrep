;(***********************************************************************)
;(*                                                                     *)
;(*                           Objective Caml                            *)
;(*                                                                     *)
;(*                Jacques Garrigue and Ian T Zimmerman                 *)
;(*                                                                     *)
;(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the GNU General Public License.                 *)
;(*                                                                     *)
;(***********************************************************************)

;(* $Id: camldebug.el 10661 2010-08-30 10:16:22Z doligez $ *)

;;; Run camldebug under Emacs
;;; Derived from gdb.el.
;;; gdb.el is Copyright (C) 1988 Free Software Foundation, Inc, and is part
;;; of GNU Emacs
;;; Modified by Jerome Vouillon, 1994.
;;; Modified by Ian T. Zimmerman, 1996.
;;; Modified by Xavier Leroy, 1997.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;itz 04-06-96 I pondered basing this on gud. The potential advantages
;;were: automatic bugfix , keymaps and menus propagation.
;;Disadvantages: gud is not so clean itself, there is little common
;;functionality it abstracts (most of the stuff is done in the
;;debugger specific parts anyway), and, most seriously, gud sees it
;;fit to add C-x C-a bindings to the _global_ map, so there would be a
;;conflict between camldebug and gdb, for instance. While it's OK to
;;assume that a sane person doesn't use gdb and dbx at the same time,
;;it's not so OK (IMHO) for gdb and camldebug.

;; Xavier Leroy, 21/02/97: adaptation to ocamldebug.

(require 'comint)
(require 'shell)
(require 'caml)
(require 'derived)
(require 'thingatpt)

;;; Variables.

(defvar camldebug-last-frame)
(defvar camldebug-delete-prompt-marker)
(defvar camldebug-filter-accumulator nil)
(defvar camldebug-last-frame-displayed-p)
(defvar camldebug-filter-function)

(defvar camldebug-prompt-pattern "^(ocd) *"
  "A regexp to recognize the prompt for ocamldebug.")

(defvar camldebug-overlay-event nil
  "Overlay for displaying the current event.")
(defvar camldebug-overlay-under nil
  "Overlay for displaying the current event.")
(defvar camldebug-event-marker nil
  "Marker for displaying the current event.")

(defvar camldebug-track-frame t
  "*If non-nil, always display current frame position in another window.")

(cond
 (window-system
  (make-face 'camldebug-event)
  (make-face 'camldebug-underline)
  (if (not (face-differs-from-default-p 'camldebug-event))
      (invert-face 'camldebug-event))
  (if (not (face-differs-from-default-p 'camldebug-underline))
      (set-face-underline-p 'camldebug-underline t))
  (setq camldebug-overlay-event (make-overlay 1 1))
  (overlay-put camldebug-overlay-event 'face 'camldebug-event)
  (setq camldebug-overlay-under (make-overlay 1 1))
  (overlay-put camldebug-overlay-under 'face 'camldebug-underline))
 (t
  (setq camldebug-event-marker (make-marker))
  (setq overlay-arrow-string "=>")))

;;; Camldebug mode.

(define-derived-mode camldebug-mode comint-mode "Inferior CDB"

  "Major mode for interacting with an inferior Camldebug process.

The following commands are available:

\\{camldebug-mode-map}

\\[camldebug-display-frame] displays in the other window
the last line referred to in the camldebug buffer.

\\[camldebug-step], \\[camldebug-back] and \\[camldebug-next], in the camldebug window,
call camldebug to step, backstep or next and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[camldebug-break].

Commands:
Many commands are inherited from comint mode.
Additionally we have:

\\[camldebug-display-frame] display frames file in other window
\\[camldebug-step] advance one line in program
C-x SPACE sets break point at current line."

  (mapcar 'make-local-variable
          '(camldebug-last-frame-displayed-p  camldebug-last-frame
            camldebug-delete-prompt-marker camldebug-filter-function
           camldebug-filter-accumulator paragraph-start))
  (setq
   camldebug-last-frame nil
   camldebug-delete-prompt-marker (make-marker)
   camldebug-filter-accumulator ""
   camldebug-filter-function 'camldebug-marker-filter
   comint-prompt-regexp camldebug-prompt-pattern
   comint-dynamic-complete-functions (cons 'camldebug-complete
                                           comint-dynamic-complete-functions)
   paragraph-start comint-prompt-regexp
   camldebug-last-frame-displayed-p t)
  (make-local-variable 'shell-dirtrackp)
  (setq shell-dirtrackp t)
  (setq comint-input-sentinel 'shell-directory-tracker))

;;; Keymaps.

(defun camldebug-numeric-arg (arg)
  (and arg (prefix-numeric-value arg)))

(defmacro def-camldebug (name key &optional doc args)

  "Define camldebug-NAME to be a command sending NAME ARGS and bound
to KEY, with optional doc string DOC.  Certain %-escapes in ARGS are
interpreted specially if present.  These are:

  %m    module name of current module.
  %d    directory of current source file.
  %c    number of current character position
  %e    text of the caml variable surrounding point.

  The `current' source file is the file of the current buffer (if
we're in a caml buffer) or the source file current at the last break
or step (if we're in the camldebug buffer), and the `current' module
name is the filename stripped of any *.ml* suffixes (this assumes the
usual correspondence between module and file naming is observed).  The
`current' position is that of the current buffer (if we're in a source
file) or the position of the last break or step (if we're in the
camldebug buffer).

If a numeric is present, it overrides any ARGS flags and its string
representation is simply concatenated with the COMMAND."

  (let* ((fun (intern (format "camldebug-%s" name))))
    (list 'progn
          (if doc
              (list 'defun fun '(arg)
                    doc
                    '(interactive "P")
                    (list 'camldebug-call name args
                          '(camldebug-numeric-arg arg))))
          (list 'define-key 'camldebug-mode-map
                (concat "\C-c" key)
                (list 'quote fun))
          (list 'define-key 'caml-mode-map
                (concat "\C-x\C-a" key)
                (list 'quote fun)))))

(def-camldebug "step"   "\C-s"  "Step one event forward.")
(def-camldebug "backstep" "\C-k" "Step one event backward.")
(def-camldebug "run"    "\C-r"  "Run the program.")
(def-camldebug "reverse" "\C-v" "Run the program in reverse.")
(def-camldebug "last"   "\C-l"  "Go to latest time in execution history.")
(def-camldebug "backtrace" "\C-t" "Print the call stack.")
(def-camldebug "finish" "\C-f"  "Finish executing current function.")
(def-camldebug "print"  "\C-p"  "Print value of symbol at point."       "%e")
(def-camldebug "display" "\C-d" "Display value of symbol at point."     "%e")
(def-camldebug "next"   "\C-n"  "Step one event forward (skip functions)")
(def-camldebug "up"     "<"  "Go up N stack frames (numeric arg) with display")
(def-camldebug "down"  ">" "Go down N stack frames (numeric arg) with display")
(def-camldebug "break"  "\C-b"  "Set breakpoint at current line."
  "@ \"%m\" # %c")

(defun camldebug-mouse-display (click)
  "Display value of $NNN clicked on."
  (interactive "e")
  (let* ((start (event-start click))
         (window (car start))
         (pos (car (cdr start)))
         symb)
    (save-excursion
      (select-window window)
      (goto-char pos)
      (setq symb (thing-at-point 'symbol))
      (if (string-match "^\\$[0-9]+$" symb)
          (camldebug-call "display" symb)))))

(define-key camldebug-mode-map [mouse-2] 'camldebug-mouse-display)

(defun camldebug-kill-filter (string)
  ;gob up stupid questions :-)
  (setq camldebug-filter-accumulator
        (concat camldebug-filter-accumulator string))
  (if (not (string-match "\\(.* \\)(y or n) "
                         camldebug-filter-accumulator)) nil
    (setq camldebug-kill-output
          (cons t (match-string 1 camldebug-filter-accumulator)))
    (setq camldebug-filter-accumulator ""))
  (if (string-match comint-prompt-regexp camldebug-filter-accumulator)
      (let ((output (substring camldebug-filter-accumulator
                               (match-beginning 0))))
        (setq camldebug-kill-output
              (cons nil (substring camldebug-filter-accumulator 0
                                   (1- (match-beginning 0)))))
        (setq camldebug-filter-accumulator "")
        output)
    ""))

(def-camldebug "kill"   "\C-k")

(defun camldebug-kill ()
  "Kill the program."
  (interactive)
  (let ((camldebug-kill-output))
    (save-excursion
      (set-buffer current-camldebug-buffer)
      (let ((proc (get-buffer-process (current-buffer)))
            (camldebug-filter-function 'camldebug-kill-filter))
        (camldebug-call "kill")
        (while (not (and camldebug-kill-output
                         (zerop (length camldebug-filter-accumulator))))
          (accept-process-output proc))))
    (if (not (car camldebug-kill-output))
        (error (cdr camldebug-kill-output))
      (sit-for 0 300)
      (camldebug-call-1 (if (y-or-n-p (cdr camldebug-kill-output)) "y" "n")))))
;;FIXME: camldebug doesn't output the Hide marker on kill

(defun camldebug-goto-filter (string)
  ;accumulate onto previous output
  (setq camldebug-filter-accumulator
        (concat camldebug-filter-accumulator string))
  (if (not (or (string-match (concat "\\(\n\\|\\`\\)[ \t]*\\([0-9]+\\)[ \t]+"
                                     camldebug-goto-position
                                     "-[0-9]+[ \t]*\\(before\\).*\n")
                             camldebug-filter-accumulator)
               (string-match (concat "\\(\n\\|\\`\\)[ \t]*\\([0-9]+\\)[ \t]+[0-9]+-"
                                     camldebug-goto-position
                                     "[ \t]*\\(after\\).*\n")
                             camldebug-filter-accumulator)))
           nil
    (setq camldebug-goto-output
          (match-string 2 camldebug-filter-accumulator))
    (setq camldebug-filter-accumulator
          (substring camldebug-filter-accumulator (1- (match-end 0)))))
  (if (not (string-match comint-prompt-regexp
                         camldebug-filter-accumulator)) nil
    (setq camldebug-goto-output (or camldebug-goto-output 'fail))
    (setq camldebug-filter-accumulator ""))
  (if (string-match "\n\\(.*\\)\\'" camldebug-filter-accumulator)
      (setq camldebug-filter-accumulator
            (match-string 1 camldebug-filter-accumulator)))
  "")

(def-camldebug "goto" "\C-g")
(defun camldebug-goto (&optional time)

  "Go to the execution time TIME.

Without TIME, the command behaves as follows: In the camldebug buffer,
if the point at buffer end, goto time 0\; otherwise, try to obtain the
time from context around point. In a caml mode buffer, try to find the
time associated in execution history with the current point location.

With a negative TIME, move that many lines backward in the camldebug
buffer, then try to obtain the time from context around point."

  (interactive "P")
  (cond
   (time
    (let ((ntime (camldebug-numeric-arg time)))
      (if (>= ntime 0) (camldebug-call "goto" nil ntime)
        (save-selected-window
          (select-window (get-buffer-window current-camldebug-buffer))
          (save-excursion
            (if (re-search-backward "^Time : [0-9]+ - pc : [0-9]+ "
                                    nil t (- 1 ntime))
                (camldebug-goto nil)
              (error "I don't have %d times in my history"
                     (- 1 ntime))))))))
   ((eq (current-buffer) current-camldebug-buffer)
      (let ((time (cond
                   ((eobp) 0)
                   ((save-excursion
                      (beginning-of-line 1)
                      (looking-at "^Time : \\([0-9]+\\) - pc : [0-9]+ "))
                    (caml-string-to-int (match-string 1)))
                   ((caml-string-to-int (camldebug-format-command "%e"))))))
        (camldebug-call "goto" nil time)))
   (t
    (let ((module (camldebug-module-name (buffer-file-name)))
          (camldebug-goto-position (int-to-string (1- (point))))
          (camldebug-goto-output) (address))
      ;get a list of all events in the current module
      (save-excursion
        (set-buffer current-camldebug-buffer)
        (let* ((proc (get-buffer-process (current-buffer)))
               (camldebug-filter-function 'camldebug-goto-filter))
          (camldebug-call-1 (concat "info events " module))
          (while (not (and camldebug-goto-output
                      (zerop (length camldebug-filter-accumulator))))
            (accept-process-output proc))
          (setq address (if (eq camldebug-goto-output 'fail) nil
                          (re-search-backward
                           (concat "^Time : \\([0-9]+\\) - pc : "
                                   camldebug-goto-output
                                   " - module "
                                   module "$") nil t)
                          (match-string 1)))))
      (if address (camldebug-call "goto" nil (caml-string-to-int address))
        (error "No time at %s at %s" module camldebug-goto-position))))))


(defun camldebug-delete-filter (string)
  (setq camldebug-filter-accumulator
        (concat camldebug-filter-accumulator string))
  (if (not (string-match
            (concat "\\(\n\\|\\`\\)[ \t]*\\([0-9]+\\)[ \t]+[0-9]+[ \t]*in "
                    (regexp-quote camldebug-delete-file)
                    ", character "
                    camldebug-delete-position "\n")
                  camldebug-filter-accumulator)) nil
    (setq camldebug-delete-output
          (match-string 2 camldebug-filter-accumulator))
    (setq camldebug-filter-accumulator
          (substring camldebug-filter-accumulator (1- (match-end 0)))))
  (if (not (string-match comint-prompt-regexp
                         camldebug-filter-accumulator)) nil
    (setq camldebug-delete-output (or camldebug-delete-output 'fail))
    (setq camldebug-filter-accumulator ""))
  (if (string-match "\n\\(.*\\)\\'" camldebug-filter-accumulator)
      (setq camldebug-filter-accumulator
            (match-string 1 camldebug-filter-accumulator)))
  "")


(def-camldebug "delete" "\C-d")

(defun camldebug-delete (&optional arg)
  "Delete the breakpoint numbered ARG.

Without ARG, the command behaves as follows: In the camldebug buffer,
try to obtain the time from context around point. In a caml mode
buffer, try to find the breakpoint associated with the current point
location.

With a negative ARG, look for the -ARGth breakpoint pattern in the
camldebug buffer, then try to obtain the breakpoint info from context
around point."

  (interactive "P")
  (cond
   (arg
    (let ((narg (camldebug-numeric-arg arg)))
      (if (> narg 0) (camldebug-call "delete" nil narg)
        (save-excursion
          (set-buffer current-camldebug-buffer)
          (if (re-search-backward "^Breakpoint [0-9]+ at [0-9]+ : file "
                                  nil t (- 1 narg))
              (camldebug-delete nil)
            (error "I don't have %d breakpoints in my history"
                     (- 1 narg)))))))
   ((eq (current-buffer) current-camldebug-buffer)
    (let* ((bpline "^Breakpoint \\([0-9]+\\) at [0-9]+ : file ")
           (arg (cond
                 ((eobp)
                  (save-excursion (re-search-backward bpline nil t))
                  (caml-string-to-int (match-string 1)))
                 ((save-excursion
                    (beginning-of-line 1)
                    (looking-at bpline))
                  (caml-string-to-int (match-string 1)))
                 ((caml-string-to-int (camldebug-format-command "%e"))))))
      (camldebug-call "delete" nil arg)))
   (t
    (let ((camldebug-delete-file
           (concat (camldebug-format-command "%m") ".ml"))
          (camldebug-delete-position (camldebug-format-command "%c")))
      (save-excursion
        (set-buffer current-camldebug-buffer)
        (let ((proc (get-buffer-process (current-buffer)))
              (camldebug-filter-function 'camldebug-delete-filter)
              (camldebug-delete-output))
          (camldebug-call-1 "info break")
          (while (not (and camldebug-delete-output
                           (zerop (length
                                   camldebug-filter-accumulator))))
            (accept-process-output proc))
          (if (eq camldebug-delete-output 'fail)
              (error "No breakpoint in %s at %s"
                     camldebug-delete-file
                     camldebug-delete-position)
            (camldebug-call "delete" nil
                            (caml-string-to-int camldebug-delete-output)))))))))

(defun camldebug-complete-filter (string)
  (setq camldebug-filter-accumulator
        (concat camldebug-filter-accumulator string))
  (while (string-match "\\(\n\\|\\`\\)\\(.+\\)\n"
                       camldebug-filter-accumulator)
    (setq camldebug-complete-list
          (cons (match-string 2 camldebug-filter-accumulator)
                camldebug-complete-list))
    (setq camldebug-filter-accumulator
          (substring camldebug-filter-accumulator
                     (1- (match-end 0)))))
  (if (not (string-match comint-prompt-regexp
                         camldebug-filter-accumulator)) nil
    (setq camldebug-complete-list
          (or camldebug-complete-list 'fail))
    (setq camldebug-filter-accumulator ""))
  (if (string-match "\n\\(.*\\)\\'" camldebug-filter-accumulator)
      (setq camldebug-filter-accumulator
            (match-string 1 camldebug-filter-accumulator)))
  "")

(defun camldebug-complete ()

  "Perform completion on the camldebug command preceding point."

  (interactive)
  (let* ((end (point))
         (command (save-excursion
                    (beginning-of-line)
                    (and (looking-at comint-prompt-regexp)
                         (goto-char (match-end 0)))
                    (buffer-substring (point) end)))
         (camldebug-complete-list nil) (command-word))

    ;; Find the word break.  This match will always succeed.
    (string-match "\\(\\`\\| \\)\\([^ ]*\\)\\'" command)
    (setq command-word (match-string 2 command))

    ;itz 04-21-96 if we are trying to complete a word of nonzero
    ;length, chop off the last character. This is a nasty hack, but it
    ;works - in general, not just for this set of words: the comint
    ;call below will weed out false matches - and it avoids further
    ;mucking with camldebug's lexer.
    (if (> (length command-word) 0)
        (setq command (substring command 0 (1- (length command)))))

    (let ((camldebug-filter-function 'camldebug-complete-filter))
      (camldebug-call-1 (concat "complete " command))
      (set-marker camldebug-delete-prompt-marker nil)
      (while (not (and camldebug-complete-list
                       (zerop (length camldebug-filter-accumulator))))
        (accept-process-output (get-buffer-process
                                (current-buffer)))))
    (if (eq camldebug-complete-list 'fail)
        (setq camldebug-complete-list nil))
    (setq camldebug-complete-list
          (sort camldebug-complete-list 'string-lessp))
    (comint-dynamic-simple-complete command-word camldebug-complete-list)))

(define-key camldebug-mode-map "\C-l" 'camldebug-refresh)
(define-key camldebug-mode-map "\t" 'comint-dynamic-complete)
(define-key camldebug-mode-map "\M-?" 'comint-dynamic-list-completions)

(define-key caml-mode-map "\C-x " 'camldebug-break)


(defvar current-camldebug-buffer nil)


;;;###autoload
(defvar camldebug-command-name "ocamldebug"
  "*Pathname for executing camldebug.")

;;;###autoload
(defun camldebug (path)
  "Run camldebug on program FILE in buffer *camldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for camldebug.  If you wish to change this, use
the camldebug commands `cd DIR' and `directory'."
  (interactive "fRun ocamldebug on file: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (pop-to-buffer (concat "*camldebug-" file "*"))
    (setq default-directory (file-name-directory path))
    (message "Current directory is %s" default-directory)
    (make-comint (concat "camldebug-" file)
                 (substitute-in-file-name camldebug-command-name)
                 nil
                 "-emacs" "-cd" default-directory file)
    (set-process-filter (get-buffer-process (current-buffer))
                        'camldebug-filter)
    (set-process-sentinel (get-buffer-process (current-buffer))
                          'camldebug-sentinel)
    (camldebug-mode)
    (camldebug-set-buffer)))

(defun camldebug-set-buffer ()
  (if (eq major-mode 'camldebug-mode)
      (setq current-camldebug-buffer (current-buffer))
    (save-selected-window (pop-to-buffer current-camldebug-buffer))))

;;; Filter and sentinel.

(defun camldebug-marker-filter (string)
  (setq camldebug-filter-accumulator
        (concat camldebug-filter-accumulator string))
  (let ((output "") (begin))
    ;; Process all the complete markers in this chunk.
    (while (setq begin
                 (string-match
                  "\032\032\\(H\\|M\\(.+\\):\\(.+\\):\\(.+\\):\\(before\\|after\\)\\)\n"
                  camldebug-filter-accumulator))
      (setq camldebug-last-frame
            (if (char-equal ?H (aref camldebug-filter-accumulator
                                     (1+ (1+ begin)))) nil
              (let ((isbefore
                     (string= "before"
                              (match-string 5 camldebug-filter-accumulator)))
                    (startpos (caml-string-to-int
                               (match-string 3 camldebug-filter-accumulator)))
                    (endpos (caml-string-to-int
                             (match-string 4 camldebug-filter-accumulator))))
                (list (match-string 2 camldebug-filter-accumulator)
                      (if isbefore startpos endpos)
                      isbefore
                      startpos
                      endpos
                      )))
            output (concat output
                           (substring camldebug-filter-accumulator
                                      0 begin))
            ;; Set the accumulator to the remaining text.
            camldebug-filter-accumulator (substring
                                          camldebug-filter-accumulator
                                          (match-end 0))
            camldebug-last-frame-displayed-p nil))


    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; camldebug-filter-accumulator until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" camldebug-filter-accumulator)
        (progn
          ;; Everything before the potential marker start can be output.
          (setq output (concat output (substring camldebug-filter-accumulator
                                                 0 (match-beginning 0))))

          ;; Everything after, we save, to combine with later input.
          (setq camldebug-filter-accumulator
                (substring camldebug-filter-accumulator (match-beginning 0))))

      (setq output (concat output camldebug-filter-accumulator)
            camldebug-filter-accumulator ""))

    output))

(defun camldebug-filter (proc string)
  (let ((output))
    (if (buffer-name (process-buffer proc))
        (let ((process-window))
          ;; it does not seem necessary to save excursion here,
          ;; since set-buffer as a temporary effect.
          ;; comint-output-filter explicitly avoids it. 
          ;; in version 23, it prevents the marker to stay at end of buffer
          ;; (save-excursion
            (set-buffer (process-buffer proc))
            ;; If we have been so requested, delete the debugger prompt.
            (if (marker-buffer camldebug-delete-prompt-marker)
                (progn
                  (delete-region (process-mark proc)
                                 camldebug-delete-prompt-marker)
                  (set-marker camldebug-delete-prompt-marker nil)))
            (setq output (funcall camldebug-filter-function string))
            ;; Don't display the specified file unless
            ;; (1) point is at or after the position where output appears
            ;; and (2) this buffer is on the screen.
            (setq process-window (and camldebug-track-frame
                                      (not camldebug-last-frame-displayed-p)
                                      (>= (point) (process-mark proc))
                                      (get-buffer-window (current-buffer))))
            ;; Insert the text, moving the process-marker.
            (comint-output-filter proc output)
          ;; ) 
          ;; this was the end of save-excursion. 
          ;; if save-excursion is used (comint-next-prompt 1) would be needed
          ;; to move the mark past then next prompt, but this is not as good
          ;; as solution.
          (if process-window
              (save-selected-window
                (select-window process-window)
                (camldebug-display-frame)))))))

(defun camldebug-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
         ;; buffer killed
         ;; Stop displaying an arrow in a source file.
         (camldebug-remove-current-event)
         (set-process-buffer proc nil))
        ((memq (process-status proc) '(signal exit))
         ;; Stop displaying an arrow in a source file.
         (camldebug-remove-current-event)
         ;; Fix the mode line.
         (setq mode-line-process
               (concat ": "
                       (symbol-name (process-status proc))))
         (let* ((obuf (current-buffer)))
           ;; save-excursion isn't the right thing if
           ;;  process-buffer is current-buffer
           (unwind-protect
               (progn
                 ;; Write something in *compilation* and hack its mode line,
                 (set-buffer (process-buffer proc))
                 ;; Force mode line redisplay soon
                 (set-buffer-modified-p (buffer-modified-p))
                 (if (eobp)
                     (insert ?\n mode-name " " msg)
                   (save-excursion
                     (goto-char (point-max))
                     (insert ?\n mode-name " " msg)))
                 ;; If buffer and mode line will show that the process
                 ;; is dead, we can delete it now.  Otherwise it
                 ;; will stay around until M-x list-processes.
                 (delete-process proc))
             ;; Restore old buffer, but don't restore old point
             ;; if obuf is the cdb buffer.
             (set-buffer obuf))))))


(defun camldebug-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the mark."
  (interactive "P")
  (camldebug-display-frame)
  (recenter arg))

(defun camldebug-display-frame ()
  "Find, obey and delete the last filename-and-line marker from CDB.
The marker looks like \\032\\032Mfilename:startchar:endchar:beforeflag\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (camldebug-set-buffer)
  (if (not camldebug-last-frame)
      (camldebug-remove-current-event)
    (camldebug-display-line (nth 0 camldebug-last-frame)
                            (nth 3 camldebug-last-frame)
                            (nth 4 camldebug-last-frame)
                            (nth 2 camldebug-last-frame)))
  (setq camldebug-last-frame-displayed-p t))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its character CHARACTER is visible.
;; Put the mark on this character in that buffer.

(defun camldebug-display-line (true-file schar echar kind)
  (let* ((pre-display-buffer-function nil) ; screw it, put it all in one screen
         (pop-up-windows t)
         (buffer (find-file-noselect true-file))
         (window (display-buffer buffer t))
         (spos) (epos) (pos))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
        (widen)
        (setq spos (+ (point-min) schar))
        (setq epos (+ (point-min) echar))
        (setq pos (if kind spos epos))
        (camldebug-set-current-event spos epos (current-buffer) kind))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
             (widen)
             (goto-char pos))))
    (set-window-point window pos)))

;;; Events.

(defun camldebug-remove-current-event ()
  (if window-system
      (progn
        (delete-overlay camldebug-overlay-event)
        (delete-overlay camldebug-overlay-under))
    (setq overlay-arrow-position nil)))

(defun camldebug-set-current-event (spos epos buffer before)
  (if window-system
      (if before
          (progn
            (move-overlay camldebug-overlay-event spos (1+ spos) buffer)
            (move-overlay camldebug-overlay-under
                          (+ spos 1) epos buffer))
        (move-overlay camldebug-overlay-event (1- epos) epos buffer)
        (move-overlay camldebug-overlay-under spos (- epos 1) buffer))
    (save-excursion
      (set-buffer buffer)
      (goto-char spos)
      (beginning-of-line)
      (move-marker camldebug-event-marker (point))
      (setq overlay-arrow-position camldebug-event-marker))))

;;; Miscellaneous.

(defun camldebug-module-name (filename)
  (substring filename (string-match "\\([^/]*\\)\\.ml$" filename) (match-end 1)))

;;; The camldebug-call function must do the right thing whether its
;;; invoking keystroke is from the camldebug buffer itself (via
;;; major-mode binding) or a caml buffer.  In the former case, we want
;;; to supply data from camldebug-last-frame.  Here's how we do it:

(defun camldebug-format-command (str)
  (let* ((insource (not (eq (current-buffer) current-camldebug-buffer)))
        (frame (if insource nil camldebug-last-frame)) (result))
    (while (and str (string-match "\\([^%]*\\)%\\([mdcep]\\)" str))
      (let ((key (string-to-char (substring str (match-beginning 2))))
            (cmd (substring str (match-beginning 1) (match-end 1)))
            (subst))
        (setq str (substring str (match-end 2)))
        (cond
         ((eq key ?m)
          (setq subst (camldebug-module-name
                       (if insource (buffer-file-name) (nth 0 frame)))))
         ((eq key ?d)
          (setq subst (file-name-directory
                       (if insource (buffer-file-name) (nth 0 frame)))))
         ((eq key ?c)
          (setq subst (int-to-string
                       (if insource (1- (point)) (nth 1 frame)))))
         ((eq key ?e)
          (setq subst (thing-at-point 'symbol))))
        (setq result (concat result cmd subst))))
    ;; There might be text left in STR when the loop ends.
    (concat result str)))

(defun camldebug-call (command &optional fmt arg)
  "Invoke camldebug COMMAND displaying source in other window.

Certain %-escapes in FMT are interpreted specially if present.
These are:

  %m    module name of current module.
  %d    directory of current source file.
  %c    number of current character position
  %e    text of the caml variable surrounding point.

  The `current' source file is the file of the current buffer (if
we're in a caml buffer) or the source file current at the last break
or step (if we're in the camldebug buffer), and the `current' module
name is the filename stripped of any *.ml* suffixes (this assumes the
usual correspondence between module and file naming is observed).  The
`current' position is that of the current buffer (if we're in a source
file) or the position of the last break or step (if we're in the
camldebug buffer).

If ARG is present, it overrides any FMT flags and its string
representation is simply concatenated with the COMMAND."

  ;; Make sure debugger buffer is displayed in a window.
  (camldebug-set-buffer)
  (message "Command: %s" (camldebug-call-1 command fmt arg)))

(defun camldebug-call-1 (command &optional fmt arg)

  ;; Record info on the last prompt in the buffer and its position.
  (save-excursion
    (set-buffer current-camldebug-buffer)
    (goto-char (process-mark (get-buffer-process current-camldebug-buffer)))
    (let ((pt (point)))
      (beginning-of-line)
      (if (looking-at comint-prompt-regexp)
          (set-marker camldebug-delete-prompt-marker (point)))))
  (let ((cmd (cond
              (arg (concat command " " (int-to-string arg)))
              (fmt (camldebug-format-command
                    (concat command " " fmt)))
              (command))))
    (process-send-string (get-buffer-process current-camldebug-buffer)
                         (concat cmd "\n"))
    cmd))


(provide 'camldebug)
