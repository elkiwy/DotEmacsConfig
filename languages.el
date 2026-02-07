;;; languages.el --- Language specific configurations -*- lexical-binding: t; -*-

;; 1. Force load the built-in treesit library immediately
;; This ensures functions like treesit-ready-p exist before anything else runs
(require 'treesit)

;; 2. Set the grammar source list (done at top-level for safety)
(setq treesit-language-source-alist
      '((jai "https://github.com/constantitus/tree-sitter-jai")))


;;TODO capisci meglio che cazzo fa sta robba
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (let ((jai-recipe
         (make-treesit-auto-recipe
          :lang 'jai
          :ts-mode 'jai-ts-mode
          :remap 'jai-mode ; maps standard jai-mode to jai-ts-mode
          :url "https://github.com/constantitus/tree-sitter-jai"
          :ext "\\.jai\\'")))
    (add-to-list 'treesit-auto-recipe-list jai-recipe))

  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))






;;-----------------------------------------------------------------------------
;; LSP (Eglot)
;(use-package eglot
;  :defer t
;  :custom
;  :config
;  (add-to-list
;   'eglot-server-programs
;   '(jai-ts-mode . ("/Users/stefanobertoli/Documents/Jails/bin/jails" "-jai_path" "/Users/stefanobertoli/Documents/jai/bin/jai"))))


(use-package lsp-mode
  :commands lsp

  :init
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(jai-ts-mode . "jai")))

  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection 
                                     '("/Users/stefanobertoli/Documents/Jails/bin/jails" 
                                       "-jai_path" "/Users/stefanobertoli/Documents/jai/bin/jai"))
                    :major-modes '(jai-ts-mode)
                    :server-id 'jails))
  
  ;; Add the hook to your major mode
  :hook (jai-ts-mode . lsp))






;;-----------------------------------------------------------------------------
;; Jai
(setq compilation-error-regexp-alist '()) ;;Workaround to fix missing variables?
(setq compilation-error-regexp-alist-alist '())
(use-package jai-ts-mode
  :straight (jai-ts-mode :type git :host github :repo "cpoile/jai-ts-mode")
  :mode "\\.jai\\'"
  :hook (jai-ts-mode . eglot-ensure)
  )







;;-----------------------------------------------------------------------------
;; PHP Support
(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . eglot-ensure))












(provide 'languages)









