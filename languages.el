;;; languages.el --- Language specific configurations -*- lexical-binding: t; -*-




;; Shim for treesit-ready-p to fix startup error with jai-ts-mode
;; The jai-ts-mode package uses this function in its autoloads, but it might not be defined during startup.
;;(unless (fboundp 'treesit-ready-p)
;;  (defun treesit-ready-p (language)
;;    (and (fboundp 'treesit-available-p)
;;         (treesit-available-p)
;;         (fboundp 'treesit-language-available-p)
;;         (treesit-language-available-p language))))


;;(load-library "treesit")


;;TODO capisci meglio che cazzo fa sta robba
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;;Add custom tree-sitter grammars to use with 'treesit-install-language-grammar'
(defvar treesit-language-source-alist
  '(
    (jai "https://github.com/constantitus/tree-sitter-jai")
   ))




;;-----------------------------------------------------------------------------
;; LSP (Eglot)
(use-package eglot
  :defer t
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (eglot-events-buffer-size 0)
  :config
  (add-to-list
   'eglot-server-programs
   '(jai-ts-mode . ("/Users/stefanobertoli/Documents/Jails/bin/jails" "-jai_path" "/Users/stefanobertoli/Documents/jai/bin/jai"))))






;;-----------------------------------------------------------------------------
;; PHP Support
(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . eglot-ensure))






;;-----------------------------------------------------------------------------
;; Jai
;;Workaround to fix missing variables?
(setq compilation-error-regexp-alist '())
(setq compilation-error-regexp-alist-alist '())
(use-package jai-ts-mode
  :straight (jai-ts-mode :type git :host github :repo "cpoile/jai-ts-mode")
  :mode "\\.jai\\'")














(provide 'languages)









