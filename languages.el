;;; languages.el --- Language specific configurations -*- lexical-binding: t; -*-


;;-----------------------------------------------------------------------------
;; LSPs
(use-package company-mode)
(add-hook 'after-init-hook 'global-company-mode)


;;-----------------------------------------------------------------------------
;; Treesit
(require 'treesit)
(setq treesit-language-source-alist
      '((jai "https://github.com/constantitus/tree-sitter-jai")))

;;Automatically handle treesitter major modes when necessary
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
  (setq lsp-enable-indentation nil)    ; Stop lsp-mode from managing indentation
  (setq lsp-enable-on-type-formatting nil) ; Stop formatting as you type
  (setq lsp-log-io t)
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("/Users/stefanobertoli/Documents/Jails/bin/jails" "-jai_path" "/Users/stefanobertoli/Documents/jai/bin/jai"))
                     :major-modes '(jai-ts-mode)
                     :server-id 'jails))
  
  ;; Add the hook to your major mode
  :hook (jai-ts-mode . lsp))





;;-----------------------------------------------------------------------------
;; Jai
;;NOTE: I had to manually fix Jails code and build a custom executable to fix diagnostics problems
(setq compilation-error-regexp-alist '()) ;;Workaround to fix missing variables?
(setq compilation-error-regexp-alist-alist '())
(use-package jai-ts-mode
  :straight (jai-ts-mode :type git :host github :repo "cpoile/jai-ts-mode")
  :mode "\\.jai\\'")








;;-----------------------------------------------------------------------------
;; PHP Support
(use-package php-mode
  :mode "\\.php\\'"
  :hook (php-mode . eglot-ensure))












(provide 'languages)









