
(add-to-list 'load-path "~/emacs/tuareg")
(add-to-list 'load-path "~/emacs/ocaml-mode")
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)
(add-hook 'tuareg-mode-hook
          '(lambda ()
             (define-key tuareg-mode-map "\M-q"
               'tuareg-indent-phrase)
             (define-key tuareg-mode-map "\C-c \C-i"
               'caml-types-show-ident)
             (define-key tuareg-mode-map [f4] 'goto-line)
             (define-key tuareg-mode-map [f5] 'compile)
             (define-key tuareg-mode-map [f6] 'recompile)
             (define-key tuareg-mode-map [f7] 'next-error)
             (auto-fill-mode 1)
             (setq tuareg-sym-lock-keywords nil)))

(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

(setq-default indent-tabs-mode nil)
