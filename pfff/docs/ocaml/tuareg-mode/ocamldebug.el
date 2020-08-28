;;; ocamldebug.el --- Run ocamldebug / camldebug under Emacs.
;; Derived from gdb.el.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         Copying is covered by the GNU General Public License.
;;
;;    This program is free software; you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation; either version 2 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;    GNU General Public License for more details.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; History:
;;
;;itz 04-06-96 I pondered basing this on gud. The potential advantages
;;were: automatic bugfix , keymaps and menus propagation.
;;Disadvantages: gud is not so clean itself, there is little common
;;functionality it abstracts (most of the stuff is done in the
;;debugger specific parts anyway), and, most seriously, gud sees it
;;fit to add C-x C-a bindings to the _global_ map, so there would be a
;;conflict between ocamldebug and gdb, for instance. While it's OK to
;;assume that a sane person doesn't use gdb and dbx at the same time,
;;it's not so OK (IMHO) for gdb and ocamldebug.

;;Albert Cohen 04-97: Patch for Tuareg support.
;;Albert Cohen 05-98: A few patches and OCaml customization.
;;Albert Cohen 09-98: XEmacs support and some improvements.
;;Erwan Jahier and Albert Cohen 11-05: support for ocamldebug 3.09.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'comint)
(require 'shell)
(require 'tuareg (expand-file-name
                  "tuareg" (file-name-directory (or load-file-name
                                                    byte-compile-current-file))))
(require 'derived)

;;; Variables.

(defvar ocamldebug-last-frame)
(defvar ocamldebug-delete-prompt-marker)
(defvar ocamldebug-filter-accumulator nil)
(defvar ocamldebug-last-frame-displayed-p)
(defvar ocamldebug-filter-function)
(defvar ocamldebug-kill-output)
(defvar ocamldebug-current-buffer nil)
(defvar ocamldebug-goto-position)
(defvar ocamldebug-goto-output)
(defvar ocamldebug-delete-file)
(defvar ocamldebug-delete-position)
(defvar ocamldebug-delete-output)
(defvar ocamldebug-complete-list)

(defvar ocamldebug-prompt-pattern "^(\\(ocd\\|cdb\\)) *"
  "A regexp to recognize the prompt for ocamldebug.")

(defvar ocamldebug-overlay-event nil
  "Overlay for displaying the current event.")
(defvar ocamldebug-overlay-under nil
  "Overlay for displaying the current event.")
(defvar ocamldebug-event-marker nil
  "Marker for displaying the current event.")

(defvar ocamldebug-track-frame t
  "*If non-nil, always display current frame position in another window.")

(cond
 ((and (fboundp 'make-overlay) window-system)
  (make-face 'ocamldebug-event)
  (make-face 'ocamldebug-underline)
  (unless (face-differs-from-default-p 'ocamldebug-event)
    (invert-face 'ocamldebug-event))
  (unless (face-differs-from-default-p 'ocamldebug-underline)
    (set-face-underline-p 'ocamldebug-underline t))
  (setq ocamldebug-overlay-event (make-overlay 1 1))
  (overlay-put ocamldebug-overlay-event 'face 'ocamldebug-event)
  (setq ocamldebug-overlay-under (make-overlay 1 1))
  (overlay-put ocamldebug-overlay-under 'face 'ocamldebug-underline))
 (t
  (setq ocamldebug-event-marker (make-marker))
  (setq overlay-arrow-string "=>")))

;;; OCamldebug mode.

(defvar ocamldebug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-l" 'ocamldebug-refresh)
    ;; This is already the default anyway!
    ;;(define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\M-?"
      ;; FIXME: This binding is wrong since comint-dynamic-list-completions
      ;; is a function, not a command.
      'comint-dynamic-list-completions)
    map))

(define-derived-mode ocamldebug-mode comint-mode "OCaml-Debugger"

  "Major mode for interacting with an ocamldebug process.

The following commands are available:

\\{ocamldebug-mode-map}

\\[ocamldebug-display-frame] displays in the other window
the last line referred to in the ocamldebug buffer.

\\[ocamldebug-step], \\[ocamldebug-back] and \\[ocamldebug-next], in the ocamldebug window,
call ocamldebug to step, backstep or next and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[ocamldebug-break].

Commands:
Many commands are inherited from comint mode.
Additionally we have:

\\[ocamldebug-display-frame] display frames file in other window
\\[ocamldebug-step] advance one line in program
C-x SPACE sets break point at current line."
  (set (make-local-variable 'ocamldebug-last-frame) nil)
  (set (make-local-variable 'ocamldebug-delete-prompt-marker) (make-marker))
  (set (make-local-variable 'ocamldebug-filter-accumulator) "")
  (set (make-local-variable 'ocamldebug-filter-function)
       #'ocamldebug-marker-filter)
  (set (make-local-variable 'comint-prompt-regexp) ocamldebug-prompt-pattern)
  (set (make-local-variable 'comint-dynamic-complete-functions)
       (cons #'ocamldebug-complete comint-dynamic-complete-functions))
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (set (make-local-variable 'ocamldebug-last-frame-displayed-p) t)
  (set (make-local-variable 'shell-dirtrackp) t)
  (add-hook 'comint-input-filter-functions 'shell-directory-tracker nil t))

;;; Keymaps.

(defun ocamldebug-numeric-arg (arg)
  (and arg (prefix-numeric-value arg)))

(defmacro def-ocamldebug (name key &optional doc args)

  "Define ocamldebug-NAME to be a command sending NAME ARGS and bound
to KEY, with optional doc string DOC.  Certain %-escapes in ARGS are
interpreted specially if present.  These are:

  %m	module name of current module.
  %d	directory of current source file.
  %c	number of current character position
  %e	text of the OCaml variable surrounding point.

  The `current' source file is the file of the current buffer (if
we're in an OCaml buffer) or the source file current at the last break
or step (if we're in the ocamldebug buffer), and the `current' module
name is the filename stripped of any *.ml* suffixes (this assumes the
usual correspondence between module and file naming is observed).  The
`current' position is that of the current buffer (if we're in a source
file) or the position of the last break or step (if we're in the
ocamldebug buffer).

If a numeric is present, it overrides any ARGS flags and its string
representation is simply concatenated with the COMMAND."

  (let* ((fun (intern (format "ocamldebug-%s" name))))
    `(progn
       ,(if doc
            `(defun ,fun (arg)
               ,doc
               (interactive "P")
               (ocamldebug-call ,name ,args
                                (ocamldebug-numeric-arg arg))))
       (define-key ocamldebug-mode-map ,(concat "\C-c" key) ',fun)
       (define-key tuareg-mode-map ,(concat "\C-x\C-a" key) ',fun))))

(def-ocamldebug "step"	"\C-s"	"Step one source line with display.")
(def-ocamldebug "run"	"\C-r"	"Run the program.")
(def-ocamldebug "reverse" "\C-v" "Run the program in reverse.")
(def-ocamldebug "last"   "\C-l"  "Go to latest time in execution history.")
(def-ocamldebug "backtrace" "\C-t" "Print the call stack.")
(def-ocamldebug "open"   "\C-o"  "Open the current module." "%m")
(def-ocamldebug "close"  "\C-c"  "Close the current module." "%m")
(def-ocamldebug "finish" "\C-f"	"Finish executing current function.")
(def-ocamldebug "print"	"\C-p"	"Print value of symbol at point."	"%e")
(def-ocamldebug "next"   "\C-n"	"Step one source line (skip functions)")
(def-ocamldebug "up"     "<"  "Go up N stack frames (numeric arg) with display")
(def-ocamldebug "down"  ">" "Go down N stack frames (numeric arg) with display")
(def-ocamldebug "break"  "\C-b"	"Set breakpoint at current line."
  "@ \"%m\" # %c")

(defun ocamldebug-kill-filter (string)
  ;gob up stupid questions :-)
  (setq ocamldebug-filter-accumulator
	(concat ocamldebug-filter-accumulator string))
  (when (string-match "\\(.* \\)(y or n) "
                      ocamldebug-filter-accumulator)
    (setq ocamldebug-kill-output
	  (cons t (match-string 1 ocamldebug-filter-accumulator)))
    (setq ocamldebug-filter-accumulator ""))
  (if (string-match comint-prompt-regexp ocamldebug-filter-accumulator)
      (let ((output (substring ocamldebug-filter-accumulator
			       (match-beginning 0))))
	(setq ocamldebug-kill-output
	      (cons nil (substring ocamldebug-filter-accumulator 0
				   (1- (match-beginning 0)))))
	(setq ocamldebug-filter-accumulator "")
	output)
    ""))

(def-ocamldebug "kill"	"\C-k")

(defun ocamldebug-kill ()
  "Kill the program."
  (interactive)
  (let ((ocamldebug-kill-output))
    (with-current-buffer ocamldebug-current-buffer
      (let ((proc (get-buffer-process (current-buffer)))
	    (ocamldebug-filter-function 'ocamldebug-kill-filter))
	(ocamldebug-call "kill")
	(while (not (and ocamldebug-kill-output
			 (zerop (length ocamldebug-filter-accumulator))))
	  (accept-process-output proc))))
    (if (not (car ocamldebug-kill-output))
	(error (cdr ocamldebug-kill-output))
      (sit-for 0 300)
      (ocamldebug-call-1 (if (y-or-n-p (cdr ocamldebug-kill-output)) "y" "n")))))
;;FIXME: ocamldebug doesn't output the Hide marker on kill

(defun ocamldebug-goto-filter (string)
  ;accumulate onto previous output
  (setq ocamldebug-filter-accumulator
	(concat ocamldebug-filter-accumulator string))
  (when (or (string-match (concat
                           "\\(\n\\|\\`\\)[ \t]*\\([0-9]+\\)[ \t]+"
                           ocamldebug-goto-position
                           "-[0-9]+[ \t]*\\(before\\).*\n")
                          ocamldebug-filter-accumulator)
            (string-match (concat
                           "\\(\n\\|\\`\\)[ \t]*\\([0-9]+\\)[ \t]+[0-9]+-"
                           ocamldebug-goto-position
                           "[ \t]*\\(after\\).*\n")
                          ocamldebug-filter-accumulator))
    (setq ocamldebug-goto-output
	  (match-string 2 ocamldebug-filter-accumulator))
    (setq ocamldebug-filter-accumulator
	  (substring ocamldebug-filter-accumulator (1- (match-end 0)))))
  (when (string-match comint-prompt-regexp
                      ocamldebug-filter-accumulator)
    (setq ocamldebug-goto-output (or ocamldebug-goto-output 'fail))
    (setq ocamldebug-filter-accumulator ""))
  (when (string-match "\n\\(.*\\)\\'" ocamldebug-filter-accumulator)
    (setq ocamldebug-filter-accumulator
          (match-string 1 ocamldebug-filter-accumulator)))
  "")

(def-ocamldebug "goto" "\C-g")
(defun ocamldebug-goto (&optional time)

  "Go to the execution time TIME.

Without TIME, the command behaves as follows: In the ocamldebug buffer,
if the point at buffer end, goto time 0\; otherwise, try to obtain the
time from context around point.  In an OCaml buffer, try to find the
time associated in execution history with the current point location.

With a negative TIME, move that many lines backward in the ocamldebug
buffer, then try to obtain the time from context around point."

  (interactive "P")
  (cond
   (time
    (let ((ntime (ocamldebug-numeric-arg time)))
      (if (>= ntime 0) (ocamldebug-call "goto" nil ntime)
	(save-selected-window
	  (select-window (get-buffer-window ocamldebug-current-buffer))
	  (save-excursion
	    (if (re-search-backward "^Time : [0-9]+ - pc : [0-9]+ "
				    nil t (- 1 ntime))
		(ocamldebug-goto nil)
	      (error "I don't have %d times in my history"
		     (- 1 ntime))))))))
   ((eq (current-buffer) ocamldebug-current-buffer)
      (let ((time (cond
		   ((eobp) 0)
		   ((save-excursion
		      (beginning-of-line 1)
		      (looking-at "^Time : \\([0-9]+\\) - pc : [0-9]+ "))
		    (string-to-number (match-string 1)))
		   ((string-to-number (ocamldebug-format-command "%e"))))))
	(ocamldebug-call "goto" nil time)))
   (t
    (let ((module (ocamldebug-module-name (buffer-file-name)))
	  (ocamldebug-goto-position (int-to-string (1- (point))))
	  (ocamldebug-goto-output) (address))
      ;get a list of all events in the current module
      (with-current-buffer ocamldebug-current-buffer
	(let* ((proc (get-buffer-process (current-buffer)))
	       (ocamldebug-filter-function 'ocamldebug-goto-filter))
	  (ocamldebug-call-1 (concat "info events " module))
	  (while (not (and ocamldebug-goto-output
		      (zerop (length ocamldebug-filter-accumulator))))
	    (accept-process-output proc))
	  (setq address (unless (eq ocamldebug-goto-output 'fail)
			  (re-search-backward
			   (concat "^Time : \\([0-9]+\\) - pc : "
				   ocamldebug-goto-output
				   " - module "
				   module "$") nil t)
			  (match-string 1)))))
      (if address (ocamldebug-call "goto" nil (string-to-number address))
	(error "No time at %s at %s" module ocamldebug-goto-position))))))


(defun ocamldebug-delete-filter (string)
  (setq ocamldebug-filter-accumulator
	(concat ocamldebug-filter-accumulator string))
  (when (string-match
         (concat "\\(\n\\|\\`\\)[ \t]*\\([0-9]+\\)[ \t]+[0-9]+[ \t]*in "
                 (regexp-quote ocamldebug-delete-file)
                 ", character "
                 ocamldebug-delete-position "\n")
         ocamldebug-filter-accumulator)
    (setq ocamldebug-delete-output
	  (match-string 2 ocamldebug-filter-accumulator))
    (setq ocamldebug-filter-accumulator
	  (substring ocamldebug-filter-accumulator (1- (match-end 0)))))
  (when (string-match comint-prompt-regexp
                      ocamldebug-filter-accumulator)
    (setq ocamldebug-delete-output (or ocamldebug-delete-output 'fail))
    (setq ocamldebug-filter-accumulator ""))
  (if (string-match "\n\\(.*\\)\\'" ocamldebug-filter-accumulator)
      (setq ocamldebug-filter-accumulator
	    (match-string 1 ocamldebug-filter-accumulator)))
  "")


(def-ocamldebug "delete" "\C-d")

(defun ocamldebug-delete (&optional arg)
  "Delete the breakpoint numbered ARG.

Without ARG, the command behaves as follows: In the ocamldebug buffer,
try to obtain the time from context around point.  In an OCaml buffer,
try to find the breakpoint associated with the current point location.

With a negative ARG, look for the -ARGth breakpoint pattern in the
ocamldebug buffer, then try to obtain the breakpoint info from context
around point."

  (interactive "P")
  (cond
   (arg
    (let ((narg (ocamldebug-numeric-arg arg)))
      (if (> narg 0) (ocamldebug-call "delete" nil narg)
	(with-current-buffer ocamldebug-current-buffer
	  (if (re-search-backward "^Breakpoint [0-9]+ at [0-9]+ : file "
				  nil t (- 1 narg))
	      (ocamldebug-delete nil)
	    (error "I don't have %d breakpoints in my history"
		     (- 1 narg)))))))
   ((eq (current-buffer) ocamldebug-current-buffer)
    (let* ((bpline "^Breakpoint \\([0-9]+\\) at [0-9]+ : file ")
	   (arg (cond
		 ((eobp)
		  (save-excursion (re-search-backward bpline nil t))
		  (string-to-number (match-string 1)))
		 ((save-excursion
		    (beginning-of-line 1)
		    (looking-at bpline))
		  (string-to-number (match-string 1)))
		 ((string-to-number (ocamldebug-format-command "%e"))))))
      (ocamldebug-call "delete" nil arg)))
   (t
    (let ((ocamldebug-delete-file
	   (concat (ocamldebug-format-command "%m") ".ml"))
	  (ocamldebug-delete-position (ocamldebug-format-command "%c")))
      (with-current-buffer ocamldebug-current-buffer
	(let ((proc (get-buffer-process (current-buffer)))
	      (ocamldebug-filter-function 'ocamldebug-delete-filter)
	      (ocamldebug-delete-output))
	  (ocamldebug-call-1 "info break")
	  (while (not (and ocamldebug-delete-output
			   (zerop (length
				   ocamldebug-filter-accumulator))))
	    (accept-process-output proc))
	  (if (eq ocamldebug-delete-output 'fail)
	      (error "No breakpoint in %s at %s"
		     ocamldebug-delete-file
		     ocamldebug-delete-position)
	    (ocamldebug-call "delete" nil
			    (string-to-number ocamldebug-delete-output)))))))))

(defun ocamldebug-complete-filter (string)
  (setq ocamldebug-filter-accumulator
	(concat ocamldebug-filter-accumulator string))
  (while (string-match "\\(\n\\|\\`\\)\\(.+\\)\n"
		       ocamldebug-filter-accumulator)
    (setq ocamldebug-complete-list
	  (cons (match-string 2 ocamldebug-filter-accumulator)
		ocamldebug-complete-list))
    (setq ocamldebug-filter-accumulator
	  (substring ocamldebug-filter-accumulator
		     (1- (match-end 0)))))
  (when (string-match comint-prompt-regexp
                      ocamldebug-filter-accumulator)
    (setq ocamldebug-complete-list
	  (or ocamldebug-complete-list 'fail))
    (setq ocamldebug-filter-accumulator ""))
  (if (string-match "\n\\(.*\\)\\'" ocamldebug-filter-accumulator)
      (setq ocamldebug-filter-accumulator
	    (match-string 1 ocamldebug-filter-accumulator)))
  "")

(defun ocamldebug-complete ()

  "Perform completion on the ocamldebug command preceding point."

  (interactive)
  (let* ((end (point))
	 (command (save-excursion
		    (beginning-of-line)
		    (and (looking-at comint-prompt-regexp)
			 (goto-char (match-end 0)))
		    (buffer-substring (point) end)))
	 (ocamldebug-complete-list nil) (command-word))

    ;; Find the word break.  This match will always succeed.
    (string-match "\\(\\`\\| \\)\\([^ ]*\\)\\'" command)
    (setq command-word (match-string 2 command))

    ;itz 04-21-96 if we are trying to complete a word of nonzero
    ;length, chop off the last character. This is a nasty hack, but it
    ;works - in general, not just for this set of words: the comint
    ;call below will weed out false matches - and it avoids further
    ;mucking with ocamldebug's lexer.
    (when (> (length command-word) 0)
      (setq command (substring command 0 (1- (length command)))))

    (let ((ocamldebug-filter-function 'ocamldebug-complete-filter))
      (ocamldebug-call-1 (concat "complete " command))
      (set-marker ocamldebug-delete-prompt-marker nil)
      (while (not (and ocamldebug-complete-list
		       (zerop (length ocamldebug-filter-accumulator))))
	(accept-process-output (get-buffer-process
				(current-buffer)))))
    (when (eq ocamldebug-complete-list 'fail)
      (setq ocamldebug-complete-list nil))
    (setq ocamldebug-complete-list
	  (sort ocamldebug-complete-list 'string-lessp))
    (comint-dynamic-simple-complete command-word ocamldebug-complete-list)))

(define-key tuareg-mode-map "\C-x " 'ocamldebug-break)

(defvar ocamldebug-command-name "ocamldebug"
  "Pathname for executing the OCaml debugger.")

;;;###autoload
(defun ocamldebug (path)
  "Run ocamldebug on program FILE in buffer *ocamldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for ocamldebug.  If you wish to change this, use
the ocamldebug commands `cd DIR' and `directory'."
  (interactive "fRun ocamldebug on file: ")
  (setq path (expand-file-name path))
  (let ((file (file-name-nondirectory path)))
    (pop-to-buffer (concat "*ocamldebug-" file "*"))
    (setq default-directory (file-name-directory path))
    (message "Current directory is %s" default-directory)
    (setq ocamldebug-command-name
	  (read-from-minibuffer "OCaml debugguer to run: "
				ocamldebug-command-name))
    (make-comint (concat "ocamldebug-" file)
		 (substitute-in-file-name ocamldebug-command-name)
		 nil
		 "-emacs" "-cd" default-directory path)
    (set-process-filter (get-buffer-process (current-buffer))
			'ocamldebug-filter)
    (set-process-sentinel (get-buffer-process (current-buffer))
			  'ocamldebug-sentinel)
    (ocamldebug-mode)
    (ocamldebug-set-buffer)))

;;;###autoload
(defalias 'camldebug 'ocamldebug)

(defun ocamldebug-set-buffer ()
  (if (eq major-mode 'ocamldebug-mode)
      (setq ocamldebug-current-buffer (current-buffer))
    (save-selected-window (pop-to-buffer ocamldebug-current-buffer))))

;;; Filter and sentinel.

(defun ocamldebug-marker-filter (string)
  (setq ocamldebug-filter-accumulator
	(concat ocamldebug-filter-accumulator string))
  (let ((output "") (begin))
    ;; Process all the complete markers in this chunk.
    (while (setq begin
		 (string-match
                  "\032\032\\(H\\|M\\(.+\\):\\(.+\\):\\(.+\\):\\(before\\|after\\)\\)\n"
		  ocamldebug-filter-accumulator))
      (setq ocamldebug-last-frame
	    (unless (char-equal ?H (aref ocamldebug-filter-accumulator
                                         (1+ (1+ begin))))
              (let ((isbefore
                     (string= "before"
                              (match-string 5 ocamldebug-filter-accumulator)))
                    (startpos (string-to-number
                               (match-string 3 ocamldebug-filter-accumulator)))
                    (endpos (string-to-number
                             (match-string 4 ocamldebug-filter-accumulator))))
                (list (match-string 2 ocamldebug-filter-accumulator)
                      (if isbefore startpos endpos)
                      isbefore
                      startpos
                      endpos
                      )))
	    output (concat output
			   (substring ocamldebug-filter-accumulator
				      0 begin))
	    ;; Set the accumulator to the remaining text.
	    ocamldebug-filter-accumulator (substring
					  ocamldebug-filter-accumulator
					  (match-end 0))
	    ocamldebug-last-frame-displayed-p nil))

    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; ocamldebug-filter-accumulator until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (if (string-match "\032.*\\'" ocamldebug-filter-accumulator)
	(progn
	  ;; Everything before the potential marker start can be output.
	  (setq output (concat output (substring ocamldebug-filter-accumulator
						 0 (match-beginning 0))))

	  ;; Everything after, we save, to combine with later input.
	  (setq ocamldebug-filter-accumulator
		(substring ocamldebug-filter-accumulator (match-beginning 0))))

      (setq output (concat output ocamldebug-filter-accumulator)
	    ocamldebug-filter-accumulator ""))

    output))

(defun ocamldebug-filter (proc string)
  (when (buffer-name (process-buffer proc))
    (let ((process-window))
      (with-current-buffer (process-buffer proc)
        ;; If we have been so requested, delete the debugger prompt.
        (when (marker-buffer ocamldebug-delete-prompt-marker)
          (delete-region (process-mark proc)
                         ocamldebug-delete-prompt-marker)
          (set-marker ocamldebug-delete-prompt-marker nil))
        (let ((output (funcall ocamldebug-filter-function string)))
          ;; Don't display the specified file unless
          ;; (1) point is at or after the position where output appears
          ;; and (2) this buffer is on the screen.
          (setq process-window (and ocamldebug-track-frame
                                    (not ocamldebug-last-frame-displayed-p)
                                    (>= (point) (process-mark proc))
                                    (get-buffer-window (current-buffer))))
          ;; Insert the text, moving the process-marker.
          (comint-output-filter proc output)))
      (when process-window
        (save-selected-window
          (select-window process-window)
          (ocamldebug-display-frame))))))

(defun ocamldebug-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (ocamldebug-remove-current-event)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (ocamldebug-remove-current-event)
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


(defun ocamldebug-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the mark."
  (interactive "P")
  (ocamldebug-display-frame)
  (recenter arg))

(defun ocamldebug-display-frame ()
  "Find, obey and delete the last filename-and-line marker from OCaml debugger.
The marker looks like \\032\\032FILENAME:CHARACTER\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (ocamldebug-set-buffer)
  (if (not ocamldebug-last-frame)
      (ocamldebug-remove-current-event)
    (ocamldebug-display-line (nth 0 ocamldebug-last-frame)
                            (nth 3 ocamldebug-last-frame)
                            (nth 4 ocamldebug-last-frame)
                            (nth 2 ocamldebug-last-frame)))
  (setq ocamldebug-last-frame-displayed-p t))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its character CHARACTER is visible.
;; Put the mark on this character in that buffer.

(defun ocamldebug-display-line (true-file schar echar kind)
  (let* ((pre-display-buffer-function nil) ; screw it, put it all in one screen
	 (pop-up-windows t)
	 (buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
         (spos) (epos) (pos))
    (with-current-buffer buffer
      (save-restriction
	(widen)
        (setq spos (+ (point-min) schar))
        (setq epos (+ (point-min) echar))
        (setq pos (if kind spos epos))
        (ocamldebug-set-current-event spos epos pos (current-buffer) kind))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window pos)))

;;; Events.

(defun ocamldebug-remove-current-event ()
  (if (and (fboundp 'make-overlay) window-system)
      (progn
        (delete-overlay ocamldebug-overlay-event)
        (delete-overlay ocamldebug-overlay-under))
    (setq overlay-arrow-position nil)))

(defun ocamldebug-set-current-event (spos epos pos buffer before)
  (if window-system
      (if before
          (progn
            (move-overlay ocamldebug-overlay-event spos (1+ spos) buffer)
            (move-overlay ocamldebug-overlay-under
                          (+ spos 1) epos buffer))
        (move-overlay ocamldebug-overlay-event (1- epos) epos buffer)
        (move-overlay ocamldebug-overlay-under spos (- epos 1) buffer))
    (with-current-buffer buffer
      (goto-char pos)
      (beginning-of-line)
      (move-marker ocamldebug-event-marker (point))
      (setq overlay-arrow-position ocamldebug-event-marker))))

;;; Miscellaneous.

(defun ocamldebug-module-name (filename)
  (substring filename (string-match "\\([^/]*\\)\\.ml$" filename) (match-end 1)))

;; The ocamldebug-call function must do the right thing whether its
;; invoking keystroke is from the ocamldebug buffer itself (via
;; major-mode binding) or an OCaml buffer.  In the former case, we want
;; to supply data from ocamldebug-last-frame.  Here's how we do it:

(defun ocamldebug-format-command (str)
  (let* ((insource (not (eq (current-buffer) ocamldebug-current-buffer)))
         (frame (if insource nil ocamldebug-last-frame))
         (result ""))
    (while (and str (string-match "\\([^%]*\\)%\\([mdcep]\\)" str))
      (let* ((key (aref str (match-beginning 2)))
             (cmd (match-string 1 str))
             (end (match-end 0))
             (subst
              (case key
                (?m
                 (ocamldebug-module-name
                  (if insource buffer-file-name (nth 0 frame))))
                (?d
                 (file-name-directory
                  (if insource buffer-file-name (nth 0 frame))))
                (?c
                 (int-to-string
                  ;; FIXME: Should this be (- (point) (point-min))?
                  ;; What happens with multibyte chars?
                  (if insource (1- (point)) (nth 1 frame))))
                (?e
                 (save-excursion
                   (skip-chars-backward "_0-9A-Za-z\277-\377")
                   (looking-at "[_0-9A-Za-z\277-\377]*")
                   (match-string 0))))))
        (setq str (substring str end))
	(setq result (concat result cmd subst))))
    ;; There might be text left in STR when the loop ends.
    (concat result str)))

(defun ocamldebug-call (command &optional fmt arg)
  "Invoke ocamldebug COMMAND displaying source in other window.

Certain %-escapes in FMT are interpreted specially if present.
These are:

  %m	module name of current module.
  %d	directory of current source file.
  %c	number of current character position
  %e	text of the OCaml variable surrounding point.

  The `current' source file is the file of the current buffer (if
we're in an OCaml buffer) or the source file current at the last break
or step (if we're in the ocamldebug buffer), and the `current' module
name is the filename stripped of any *.ml* suffixes (this assumes the
usual correspondence between module and file naming is observed).  The
`current' position is that of the current buffer (if we're in a source
file) or the position of the last break or step (if we're in the
ocamldebug buffer).

If ARG is present, it overrides any FMT flags and its string
representation is simply concatenated with the COMMAND."

  ;; Make sure debugger buffer is displayed in a window.
  (ocamldebug-set-buffer)
  (message "Command: %s" (ocamldebug-call-1 command fmt arg)))

(defun ocamldebug-call-1 (command &optional fmt arg)
  ;; Record info on the last prompt in the buffer and its position.
  (with-current-buffer ocamldebug-current-buffer
    (goto-char (process-mark (get-buffer-process ocamldebug-current-buffer)))
    (let ((pt (point)))
      (beginning-of-line)
      (when (looking-at comint-prompt-regexp)
        (set-marker ocamldebug-delete-prompt-marker (point)))))
  (let ((cmd (cond
	      (arg (concat command " " (int-to-string arg)))
	      (fmt (ocamldebug-format-command
		    (concat command " " fmt)))
	      (command))))
    (process-send-string (get-buffer-process ocamldebug-current-buffer)
			 (concat cmd "\n"))
    cmd))


(provide 'ocamldebug)
;;; ocamldebug.el ends here
