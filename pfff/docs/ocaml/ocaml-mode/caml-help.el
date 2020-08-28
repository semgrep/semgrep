;(***********************************************************************)
;(*                                                                     *)
;(*                           Objective Caml                            *)
;(*                                                                     *)
;(*            Didier Remy, projet Cristal, INRIA Rocquencourt          *)
;(*                                                                     *)
;(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the GNU General Public License.                 *)
;(*                                                                     *)
;(***********************************************************************)

;(* $Id: caml-help.el 10323 2010-04-28 11:11:07Z remy $ *)

;; caml-info.el --- contextual completion and help to caml-mode

;; Didier Remy, November 2001.

;; This provides two functions completion and help
;; look for caml-complete and caml-help

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  This is a preliminary version.
;;
;;  Possible improvements?
;;   - dump some databaes: Info, Lib, ...
;;   - accept a search path for local libraries instead of current dir
;;     (then distinguish between different modules lying in different
;;     directories)
;;   - improve the construction for info files.
;;
;;  Abstract over
;;   - the viewing method and the database, so that the documentation for
;;     and identifier could be search in
;;       * info / html / man / mli's sources
;;       * viewed in emacs or using an external previewer.
;;
;;  Take all identifiers (labels, Constructors, exceptions, etc.)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-and-compile
  (if (and (boundp 'running-xemacs) running-xemacs)
      (require 'caml-xemacs)
    (require 'caml-emacs)))

;; Loading or building databases.
;;

;; variables to be customized

(defvar ocaml-lib-path 'lazy
  "Path list for ocaml lib sources (mli files)

'lazy means ask ocaml to find it for your at first use.")
(defun ocaml-lib-path ()
  "Computes if necessary and returns the path for ocaml libs"
  (if (listp ocaml-lib-path) nil
    (setq ocaml-lib-path
          (split-string
           (shell-command-to-string
            (or
             (and (boundp 'inferior-caml-program)
                      (string-match "\\([^ ]*/ocaml\\)\\( \\|$\\)"
                       inferior-caml-program)
                      (let ((file
                             (concat (match-string 1 inferior-caml-program)
                                     "c")))
                        (and (file-executable-p file)
                             (concat file " -where"))))
             "ocamlc -where")))))
    ocaml-lib-path)



;; General purpose auxiliary functions

(defun ocaml-capitalize (s)
  (concat (capitalize (substring s 0 1)) (substring s 1)))

(defun ocaml-uncapitalize (s)
  (if (> (length s) 0)
      (concat (downcase (substring s 0 1)) (substring s 1))
    s))

(defun iter (f l) (while (consp l) (apply f (list (car l))) (setq l (cdr l))))

(defun ocaml-find-files (path filter &optional depth split)
  (let* ((path-string
          (if (stringp path)
              (if (file-directory-p path) path nil)
            (mapconcat '(lambda (d) (if (file-directory-p d) d))
                       path " ")))
         (command
          (and path-string
               (concat "find " path-string
                       " '(' " filter " ')' "
                       (if depth (concat " -maxdepth " (int-to-string depth)))
                       (if split nil " -printf '%\p '")
                       )))
          (files
           (and command (shell-command-to-string command))))
         (if (and split (stringp files)) (split-string files "\n") files)
         ))

;; Specialized auxiliary functions


;; Global table of modules contents of modules loaded lazily.

(defvar ocaml-module-alist 'lazy
  "A-list of modules with how and where to find help information.
  'delay means non computed yet")

(defun ocaml-add-mli-modules (modules tag &optional path)
  (let ((files
         (ocaml-find-files (or path (ocaml-lib-path))
                           "-type f -name '*.mli'" 1 t)))
    (while (consp files)
      (if (string-match "\\([^/]*\\).mli" (car files))
          (let* ((module (ocaml-capitalize (match-string 1 (car files))))
                 (dir (file-name-directory (car files)))
                 (dirp (member dir (ocaml-lib-path))))
            (if (and (consp dirp) (string-equal dir (car dirp)))
                (setq dir (car dirp)))
            (if (assoc module modules) nil
              (setq modules
                    (cons (cons module (cons (cons tag dir) 'lazy)) modules))
              )))
      (setq files (cdr files)))
    modules))

(defun ocaml-add-path (dir &optional path)
  "Extend  ocaml-module-alist with modules of DIR relative to PATH"
  (interactive "D")
  (let* ((old (ocaml-lib-path))
         (new
          (if (file-name-absolute-p dir) dir
            (concat
             (or (find-if '(lambda (p) (file-directory-p (concat p  "/" dir)))
                      (cons default-directory old))
                 (error "Directory not found"))
             "/" dir))))
    (setq ocaml-lib-path (cons (car old) (cons new (cdr old))))
    (setq ocaml-module-alist
          (ocaml-add-mli-modules (ocaml-module-alist) 'lib new))))

(defun ocaml-module-alist ()
  "Call by need value of variable ocaml-module-alist"
  (if (listp ocaml-module-alist)
      nil
    ;; build list of mli files
    (setq ocaml-module-alist (ocaml-add-mli-modules nil 'lib))
    ;; dumping information ? TODO
    )
  ocaml-module-alist)

(defun ocaml-get-or-make-module (module &optional tag)
  (let ((info (assoc module (ocaml-module-alist))))
    (if info nil
      (setq info (cons module (cons (cons 'local default-directory) 'lazy)))
      (setq ocaml-module-alist (cons info ocaml-module-alist))
      )
    info))

;; Symbols of module are lazily computed

(defun ocaml-module-filename (module)
  (let ((module (ocaml-uncapitalize module)) (name))
    (if (file-exists-p (setq name (concat module ".mli"))) nil
      (let ((tmp (ocaml-lib-path)))
        (while (consp tmp)
          (setq name (concat (car tmp) "/" module ".mli"))
          (if (file-exists-p name) (setq tmp nil)
            (setq name nil)))))
    name))

(defun ocaml-module-symbols (module-info)
  (let* ((module (car module-info))
         (tail (and module-info (cdr module-info)))
         (tag (caar tail))
         (dir (cdar tail))
         (file)
         (alist))
    (if (listp (cdr tail))
        (cdr tail)
      (if (equal tag 'info)
          (setq dir (car ocaml-lib-path)) ; XXX to be fixed
        )
      (setq file (concat dir "/" (ocaml-uncapitalize module) ".mli"))
      (message file)
      (save-window-excursion
        (set-buffer (get-buffer-create "*caml-help*"))
        (if (and file (file-exists-p file))
            (progn
              (message "Scanning module %s" file)
              (insert-file-contents file))
          (message "Module %s not found" module))
        (while (re-search-forward
                "\\([ \t]*val\\|let\\|external\\|  [|]\\) \\([a-zA-Z_0-9'][a-zA-Z_0-9']*\\)\\|^  *[{]* \\([a-z_][A-Za-z_0-9]*\\) : [^;\n][^;\n]*;"
                (point-max) 'move)
          (pop-to-buffer (current-buffer))
          (setq alist (cons (or (match-string 2) (match-string 3)) alist)))
        (erase-buffer)
        )
      (setcdr tail alist)
      alist)
      ))

;; Local list of visible modules.

(defvar ocaml-visible-modules 'lazy
  "A-list of open modules, local to every file.")
(make-variable-buffer-local 'ocaml-visible-modules)
(defun ocaml-visible-modules ()
  (if (listp ocaml-visible-modules) nil
    (progn
      (setq ocaml-visible-modules
            (list (ocaml-get-or-make-module "Pervasives")))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^ *open  *\\([A-Z][a-zA-Z'_0-9]*\\)"
                                  (point-max) t)
          (let ((module (match-string 1)))
            (if (assoc module ocaml-visible-modules) nil
              (setq ocaml-visible-modules
                    (cons (ocaml-get-or-make-module module)
                          ocaml-visible-modules)))))
        )))
  ocaml-visible-modules)

(defun ocaml-open-module (arg)
  "*Make module of name ARG visible whe ARG is a string.
When call interactively, make completion over known modules."
  (interactive "P")
  (if (not (stringp arg))
      (let ((modules (ocaml-module-alist)))
        (setq arg
              (completing-read "Open module: " modules))))
  (if (and (stringp arg) (not (equal arg "")))
      (progn
        (if (assoc arg (ocaml-visible-modules))
            (ocaml-close-module arg))
        (setq ocaml-visible-modules
              (cons (ocaml-get-or-make-module arg) (ocaml-visible-modules)))
        ))
  (message "%S" (mapcar 'car (ocaml-visible-modules))))

(defun ocaml-close-module (arg)
  "*Close module of name ARG when ARG is a string.
When call interactively, make completion over visible modules.
Otherwise if ARG is true, close all modules and reset to default. "
  (interactive "P")
  (if (= (prefix-numeric-value arg) 4)
      (setq ocaml-visible-modules 'lazy)
    (let* ((modules (ocaml-visible-modules)))
      (if (null modules) (error "No visible module to close"))
      (unless (stringp arg)
        (setq arg
              (completing-read
               (concat "Close module ["  (caar modules) "] : ")
               modules))
        (if (equal arg "") (setq arg (caar modules))))
      (setq ocaml-visible-modules
            (remove-if '(lambda (m) (equal (car m) arg))
                       ocaml-visible-modules))
      ))
  (message "%S" (mapcar 'car (ocaml-visible-modules))))


;; Look for identifiers around point

(defun ocaml-qualified-identifier (&optional show)
  "Search for a qualified identifier (Path. entry) around point.

Entry may be nil.
Currently, the path may only be nil or a single Module.
For paths is of the form Module.Path', it returns Module
and always nil for entry.

If defined Module and Entry are represented by a region in the buffer,
and are nil otherwise.

For debugging purposes, it returns the string Module.entry if called
with an optional non-nil argument.
"
  (save-excursion
    (let ((module) (entry))
      (if (looking-at "[ \n]") (skip-chars-backward " "))
      (if (re-search-backward
           "\\([^A-Za-z0-9_.']\\|\\`\\)\\([A-Za-z0-9_']*[.]\\)*[A-Za-z0-9_']*\\="
           (- (point) 100) t)
          (progn
            (or (looking-at "\\`[A-Za-z)-9_.]") (forward-char 1))
            (if (looking-at "\\<\\([A-Za-z_][A-Za-z0-9_']*\\)[.]")
                (progn
                  (setq module (cons (match-beginning 1) (match-end 1)))
                  (goto-char (match-end 0))))
            (if (looking-at "\\<\\([A-Za-z_][A-Za-z0-9_']*\\)\\>")
                (setq entry (cons (match-beginning 1) (match-end 1))))))
      (if show
          (concat
           (and module (buffer-substring (car module) (cdr module)))
           "."
           (and entry (buffer-substring (car entry) (cdr entry))))
      (cons module entry))
    )))

;; completion around point

(defun ocaml-completion (pattern module)
  (let ((list
         (or
          (and module
               (list
                (or (assoc module (ocaml-module-alist))
                    (error "Unknown module %s" module))))
          (ocaml-visible-modules))))
    (message "Completion from %s" (mapconcat 'car list " "))
    (if (null pattern)
        (apply 'append (mapcar 'ocaml-module-symbols list))
      (let ((pat (concat "^" (regexp-quote pattern))) (res))
        (iter
         '(lambda (l)
            (iter '(lambda (x)
                     (if (string-match pat (car l))
                         (if (member x res) nil (setq res (cons x res)))))
                  (ocaml-module-symbols l)))
         list)
        res)
      )))

(defun caml-complete (arg)
  "Does completion for OCaml identifiers qualified.

It attemps to recognize an qualified identifier Module . entry
around point using function \\[ocaml-qualified-identifier].

If Module is defined, it does completion for identifier in Module.

If Module is undefined, it does completion in visible modules.
Then, if completion fails, it does completion among  all modules
where identifier is defined."
  (interactive "p")
  (let* ((module-entry (ocaml-qualified-identifier)) (entry)
         (module)
         (beg) (end) (pattern))
    (if (car module-entry)
        (progn
          (setq module
                (buffer-substring (caar module-entry) (cdar module-entry)))
          (or (assoc module (ocaml-module-alist))
              (and (setq module
                         (completing-read "Module: " (ocaml-module-alist)
                                          nil nil module))
                   (save-excursion
                     (goto-char (caar module-entry))
                     (delete-region (caar module-entry) (cdar module-entry))
                     (insert module) t)
                   (setq module-entry (ocaml-qualified-identifier))
                   (car module-entry)
                   (progn (setq entry (cdr module-entry)) t))
              (error "Unknown module %s" module))))
    (if (consp (cdr module-entry))
        (progn
          (setq beg (cadr module-entry))
          (setq end (cddr module-entry)))
      (if (and module
           (save-excursion
            (goto-char (cdar module-entry))
            (looking-at " *[.]")))
          (progn
            (setq beg (match-end 0))
            (setq end beg))))
    (if (not (and beg end))
        (error "Did not find anything to complete around point")

      (setq pattern (buffer-substring beg end))
      (let* ((all-completions (ocaml-completion pattern module))
             (completion
              (try-completion pattern (mapcar 'list all-completions))))
        (cond ((eq completion t))

              ((null completion)
               (let*
                   ((modules (ocaml-find-module pattern))
                    (visible (intersection modules (ocaml-visible-modules)))
                    (hist)
                    (module
                     (cond
                      ((null modules)
                       nil)
                      ((equal (length modules) 1)
                       (caar modules))
                      ((equal (length visible) 1)
                       (caar visible))
                      (t
                       (setq hist (mapcar 'car modules))
                       (completing-read "Module: " modules nil t
                                        "" (cons hist 0)))
                      )))
                 (if (null module)
                     (error "Can't find completion for \"%s\"" pattern)
                   (message "Completion found in module %s" module)
                   (if (and (consp module-entry) (consp (cdr module-entry)))
                       (delete-region (caar module-entry) end)
                     (delete-region beg end))
                   (insert module "." pattern))))

              ((not (string-equal pattern completion))
               (delete-region beg end)
               (goto-char beg)
               (insert completion))

              (t
                (with-output-to-temp-buffer "*Completions*"
                  (display-completion-list all-completions))
                ))
               ))))


;; Info files (only in ocamldoc style)


(defvar ocaml-info-prefix "ocaml-lib"
  "Prefix of ocaml info files describing library modules.
Suffix .info will be added to info files.
Additional suffix .gz may be added if info files are compressed.
")
;;

(defun ocaml-hevea-info-add-entries (entries dir name)
  (let*
      ((filter
        (concat "-type f -regex '.*/" name
                "\\(.info\\|\\)\\(-[0-9]*\\|\\)\\([.]gz\\|\\)'"
                ))
       (section-regexp
        "\\* \\(Section [1-9][0-9--]*\\)::[ \t][ \t]*Module *\\([A-Z][A-Za-z_0-9]*\\)")
       (files (ocaml-find-files dir filter))
       (command))
    ;; scanning info files
    (if (or (null files)
            (not (stringp files))
            (string-match files "^ *$"))
        (message "No info file found: %s." (mapconcat 'identity files " "))
      (message "Scanning info files %s." files)
      (save-window-excursion
        (set-buffer (get-buffer-create "*caml-help*"))
        (setq command
              (concat "zcat -f " files
                      " | grep -e '" section-regexp "'"))
        (message "Scanning files with: %s" command)
        (or (shell-command command (current-buffer))
            (error "Error while scanning"))
        (goto-char (point-min))
        (while (re-search-forward section-regexp (point-max) t)
          (let* ((module (match-string 2))
                 (section (match-string 1)))
            ;; (message "%s %s" module section)
            (if (assoc module entries) nil
              (setq entries
                    (cons (cons module (concat "(" name ")" section))
                          entries))
              )))
        (let ((buf (get-buffer "*caml-help*")))
          (if buf (kill-buffer buf)))))
    entries))

(defun ocaml-hevea-info ()
  "The default way to create an info data base from the value
of \\[Info-default-directory-list] and the base name \\[ocaml-info-name]
of files to look for.

This uses info files produced by HeVeA.
"
  (let ((collect) (seen))
    (iter '(lambda (d)
             (if (member d seen) nil
               (setq collect
                     (ocaml-hevea-info-add-entries
                      collect d ocaml-info-prefix))
               (setq done (cons d seen))))
          Info-directory-list)
    collect))

(defun ocaml-ocamldoc-info-add-entries (entries dir name)
  (let*
      ((module-regexp "^Node: \\([A-Z][A-Za-z_0-9]*\\)[^ ]")
       (command
        (concat
         "find " dir " -type f -regex '.*/" name
         "\\(.info\\|\\)\\([.]gz\\|\\)' -print0"
         " | xargs -0 zcat -f | grep '" module-regexp "'")))
    (message "Scanning info files in %s" dir)
    (save-window-excursion
      (set-buffer (get-buffer-create "*caml-help*"))
      (or (shell-command command (current-buffer))
          (error "Could not run:%s" command))
      (goto-char (point-min))
      (while (re-search-forward module-regexp (point-max) t)
        (if (equal (char-after (match-end 1)) 127)
            (let* ((module (match-string 1)))
              (if (assoc module entries) nil
                (setq entries
                      (cons (cons module (concat "(" name ")" module))
                            entries))
                ))))
      ; (kill-buffer (current-buffer))
      )
    entries))

(defun ocaml-ocamldoc-info ()
  "The default way to create an info data base from the value
of \\[Info-default-directory-list] and the base name \\[ocaml-info-name]
of files to look for.

This uses info files produced by ocamldoc."
  (require 'info)
  (let ((collect) (seen))
    (iter '(lambda (d)
             (if (member d seen) nil
               (setq collect
                     (ocaml-ocamldoc-info-add-entries collect d
                                                      ocaml-info-prefix))
               (setq done (cons d seen))))
          Info-directory-list)
    collect))

;; Continuing

(defvar ocaml-info-alist 'ocaml-ocamldoc-info
  "A-list binding module names to info entries:

  nil means do not use info.

  A function to build the list lazily (at the first call). The result of
the function call will be assign permanently to this variable for future
uses. We provide two default functions \\[ocaml-info-default-function]
(info produced by HeVeA is the default) and \\[ocaml-info-default-function]
(info produced by ocamldoc).

  Otherwise, this value should be an alist binding module names to info
entries of the form to \"(entry)section\" be taken by the \\[info]
command. An entry may be an info module or a complete file name."
)

(defun ocaml-info-alist ()
  "Call by need value of variable ocaml-info-alist"
  (cond
   ((listp ocaml-info-alist))
   ((functionp ocaml-info-alist)
    (setq ocaml-info-alist (apply ocaml-info-alist nil)))
   (t
    (error "wrong type for ocaml-info-alist")))
  ocaml-info-alist)

;; help around point

(defun ocaml-find-module (symbol &optional module-list)
  (let ((list (or module-list (ocaml-module-alist)))
        (collect))
    (while (consp list)
      (if (member symbol (ocaml-module-symbols (car list)))
          (setq collect (cons (car list) collect)))
      (setq list (cdr list)))
    (nreverse collect)
    ))

(defun ocaml-buffer-substring (region)
  (and region (buffer-substring-no-properties (car region) (cdr region))))

;; Help function.


(defun ocaml-goto-help (&optional module entry same-window)
  "Searches info manual for MODULE and ENTRY in MODULE.
If unspecified, MODULE and ENTRY are inferred from the position in the
current buffer using \\[ocaml-qualified-identifier]."
  (interactive)
  (let ((window (selected-window))
        (info-section (assoc module (ocaml-info-alist))))
    (if info-section
        (caml-info-other-window (cdr info-section))
      (ocaml-visible-modules)
      (let* ((module-info
              (or (assoc module (ocaml-module-alist))
                  (and (file-exists-p
                        (concat (ocaml-uncapitalize module) ".mli"))
                       (ocaml-get-or-make-module module))))
             (location (cdr (cadr module-info))))
        (cond
         (location
          (let ((file (concat location (ocaml-uncapitalize module) ".mli")))
            (if (window-live-p same-window)
                (progn (select-window same-window)
                       (view-mode-exit view-return-to-alist view-exit-action))
              ;; (view-buffer (find-file-noselect file) 'view))
              )
            (view-file-other-window file)
            (bury-buffer (current-buffer))))
         (info-section (error "Aborted"))
         (t (error "No help for module %s" module))))
      )
    (if (stringp entry)
        (let ((here (point))
              (case-fold-search nil))
          (goto-char (point-min))
          (if (or (re-search-forward
                   (concat "\\(val\\|exception\\|type\\|external\\|[|{;]\\) +"
                           (regexp-quote entry))
                   (point-max) t)
                  (re-search-forward
                   (concat "type [^{]*{[^}]*" (regexp-quote entry) " :")
                   (point-max) t)
                  (progn
                    (if (window-live-p window) (select-window window))
                    (error "Entry %s not found in module %s"
                           entry module))
                  ;; (search-forward entry (point-max) t)
                  )
              (recenter 1)
            (progn
              (message "Help for entry %s not found in module %s"
                       entry module)
              (goto-char here)))
          ))
    (ocaml-link-activate (cdr info-section))
    (if (window-live-p window) (select-window window))
    ))

(defun caml-help (arg)
  "Find documentation for OCaml qualified identifiers.

It attemps to recognize an qualified identifier of the form
``Module . entry'' around point using function `ocaml-qualified-identifier'.

If Module is undetermined it is temptatively guessed from the identifier name
and according to visible modules. If this is still unsucessful,  the user is
then prompted for a Module name.

The documentation for Module is first seach in the info manual if available,
then in the ``module.mli'' source file. The entry is then searched in the
documentation.

Visible modules are computed only once, at the first call.
Modules can be made visible explicitly with `ocaml-open-module' and
hidden with `ocaml-close-module'.

Prefix arg 0 forces recompilation of visible modules (and their content)
from the file content.

Prefix arg 4 prompts for Module and identifier instead of guessing values
from the possition of point in the current buffer.
"
  (interactive "p")
  (let ((module) (entry) (module-entry))
    (cond
     ((= arg 4)
      (or (and
           (setq module
                (completing-read "Module: " (ocaml-module-alist)
                                 nil t "" (cons 'hist 0)))
           (not (string-equal module "")))
          (error "Quit"))
      (let ((symbols
             (mapcar 'list
                     (ocaml-module-symbols
                      (assoc module (ocaml-module-alist))))))
        (setq entry (completing-read "Value: " symbols nil t)))
      (if (string-equal entry "") (setq entry nil))
      )
     (t
      (if (= arg 0) (setq ocaml-visible-modules 'lazy))
      (setq module-entry (ocaml-qualified-identifier))
      (setq entry (ocaml-buffer-substring (cdr module-entry)))
      (setq module
            (or (ocaml-buffer-substring (car module-entry))
                (let ((modules
                       (or (ocaml-find-module entry (ocaml-visible-modules))
                           (ocaml-find-module entry)))
                      (hist) (default))
                  (cond
                   ((null modules)
                    (error "No module found for entry %s" entry))
                   ((equal (length modules) 1)
                    (caar modules))
                   (t
                    (setq hist (mapcar 'car modules))
                    (setq default (car hist))
                    (setq module
                          (completing-read
                           (concat "Module: "
                                   (and default (concat "[" default "] ")))
                           modules nil t "" (cons 'hist 0)))
                    (if (string-equal module "") default module))
                   ))))
      ))
     (message "Help for %s%s%s" module (if entry "." "") (or entry ""))
     (ocaml-goto-help module entry)
     ))

;; auto-links

(defconst ocaml-link-regexp
  "\\(type\\|and\\) \\('[a-z] +\\|(\\('[a-z], *\\)*'[a-z])\\|\\) *\\([a-zA-Z0-9_]*\\)\\( *$\\| =\\)")
(defconst ocaml-longident-regexp
  "\\([A-Z][a-zA-Z_0]*\\)[.]\\([a-zA-Z][A-Za-z0-9_]*\\)")

(defvar ocaml-links nil
  "Local links in the current of last info node or interface file.

The car of the list is a key that indentifies the module to prevent
recompilation when next help command is relative to the same module.
The cdr is a list of elments, each of which is an string and a pair of
buffer positions."
)
(make-variable-buffer-local 'ocaml-links)

(defun ocaml-info-links (section)
  (cdr
   (if (and ocaml-links section (equal (car ocaml-links) section))
       ocaml-links
     (save-excursion
       (goto-char (point-min))
       (let ((regexp (concat (if (equal major-mode 'Info-mode) "^ - " "^")
                             ocaml-link-regexp))
             (all))
         (while (re-search-forward regexp (point-max) t)
           (setq all
                 (cons (cons (match-string 4)
                             (cons (match-beginning 4)
                                   (match-end 4)))
                       all)))
         (setq ocaml-links (cons section all))
         )))))

(defvar ocaml-link-map (make-sparse-keymap))
(define-key ocaml-link-map [mouse-2] 'ocaml-link-goto)

(defun ocaml-link-goto (click)
  (interactive "e")
  (let* ((pos (caml-event-point-start click))
         (win (caml-event-window click))
         (buf (window-buffer win))
         (window (selected-window))
         (link))
    (setq link
          (with-current-buffer buf
            (buffer-substring
             (previous-single-property-change (+ pos 1) 'local-map
                                                   buf (- pos 100))
             (next-single-property-change pos  'local-map
                                        buf (+ pos 100)))))
    (if (string-match (concat "^" ocaml-longident-regexp "$") link)
        (ocaml-goto-help (match-string 1 link) (match-string 2 link) win)
      (if (not (equal (window-buffer window) buf))
          (switch-to-buffer-other-window buf))
      (if (setq link (assoc link (cdr ocaml-links)))
          (progn
            (goto-char (cadr link))
            (recenter 1)))
      (if (window-live-p window) (select-window window))
      )))

(cond
 ((and (x-display-color-p)
       (not (memq 'ocaml-link-face (face-list))))
  (make-face 'ocaml-link-face)
  (set-face-foreground 'ocaml-link-face "Purple")))


(defun ocaml-link-activate (section)
  (let ((links (ocaml-info-links section)))
    (if links
        (let ((regexp (concat "[^A-Za-z0-9'_]\\("
                              ocaml-longident-regexp "\\|"
                              (mapconcat 'car links "\\|")
                              "\\)[^A-Za-z0-9'_]"))
              (case-fold-search nil))
          (save-excursion
            (goto-char (point-min))
            (let ((buffer-read-only nil)
                  ;; use of dynamic scoping, need not be restored!
                  (modified-p (buffer-modified-p)))
              (unwind-protect
                  (save-excursion
                    (goto-char (point-min))
                    (while (re-search-forward regexp (point-max) t)
                      (put-text-property (match-beginning 1) (match-end 1)
                                         'mouse-face 'highlight)
                      (put-text-property (match-beginning 1) (match-end 1)
                                         'local-map ocaml-link-map)
                      (if (x-display-color-p)
                          (put-text-property (match-beginning 1) (match-end 1)
                                             'face 'ocaml-link-face)))
                    )
                ;; need to restore flag if buffer was unmodified.
                (unless modified-p (set-buffer-modified-p nil))
                ))
            )))))



;; bindings ---now in caml.el

; (and
;  (boundp 'caml-mode-map)
;  (keymapp caml-mode-map)
;  (progn
;    (define-key caml-mode-map [?\C-c?i] 'ocaml-add-path)
;    (define-key caml-mode-map [?\C-c?]] 'ocaml-close-module)
;    (define-key caml-mode-map [?\C-c?[] 'ocaml-open-module)
;    (define-key caml-mode-map [?\C-c?\C-h] 'caml-help)
;    (define-key caml-mode-map [?\C-c?\t] 'caml-complete)
;    (let ((map (lookup-key caml-mode-map [menu-bar caml])))
;      (and
;       (keymapp map)
;       (progn
;         (define-key map [separator-help] '("---"))
;         (define-key map [open] '("Open add path" . ocaml-add-path ))
;         (define-key map [close]
;           '("Close module for help" . ocaml-close-module))
;         (define-key map [open] '("Open module for help" . ocaml-open-module))
;         (define-key map [help] '("Help for identifier" . caml-help))
;         (define-key map [complete] '("Complete identifier" . caml-complete))
;         )
;    ))))


(provide 'caml-help)
