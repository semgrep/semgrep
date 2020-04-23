
;; inspired by igrep.el's igrep-read-regex
(defun tbgX-read-string (prompt)
  "Read and return a string from the minibuffer.
PROMPT is used as the prompt."
  (let* ((symbol (symbol-at-point))
         (default-value (if symbol (symbol-name symbol) nil)))
    (list
     (read-string (concat prompt ": ") default-value 'tbgX-history))))


(defun fb-argmax (ffun lst)
  (if (not lst) nil
    (reduce (lambda (lhs rhs)
              (let ((lhsval (funcall ffun lhs))
                    (rhsval (funcall ffun rhs)))
                (if (> lhsval rhsval) lhs rhs)))
            (cdr lst)
            :initial-value (car lst))))


(defun tbgX-do-grep (args)
  "Run grep with args, doing quoting"
  (let ((default-directory (or xbgX-exec-dir default-directory)))
    (grep (concat grep-command " -- " (shell-quote-argument args)))))

(defun run-git-grep (cmd regex)
  (let ((grep-use-null-device nil)
        (grep-command cmd)
        (root-dir (fb-find-repo-root (buffer-file-name))))
    (with-working-directory root-dir (tbgX-do-grep regex))
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) (list root-dir)))))

(defun sgrep (pattern)
  "run sgrep interactively"
  (interactive (tbgX-read-string "pattern"))
  ;; heuristic to figure out what the inteded filter string is.  Take
  ;; the longest identifier-like thing
  (let ((prefilter (fb-argmax 'length (split-string pattern "[^a-zA-Z_0-9]+"))))
    (run-git-grep
     (concat "find . \\( -name \".git\" -o -name \".hg\" \\)  -prune -o -name \"*.php\" -print0 | "
             "xargs --null grep -l " (shell-quote-argument prefilter) " | "
             "xargs " sgrep-path " -emacs -e ")
     pattern)))
