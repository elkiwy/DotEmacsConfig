;;; languages.el --- Language specific configurations -*- lexical-binding: t; -*-


;;-----------------------------------------------------------------------------
;; Completion Engine
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
  :mode "\\.php\\'")




;;-----------------------------------------------------------------------------
;; GLSL Support
(use-package glsl-mode
  :mode "\\.hlsl\\'")






;;-----------------------------------------------------------------------------
;; Org Mode
(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-hide-leading-stars t) ; Hides all but one star for a cleaner look
  (setq org-adapt-indentation nil)) ; Ensures Org doesn't add real spaces


(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  ;; This ensures >> and << work for headings and lists
  (require 'evil-org-agenda)
  (evil-org-set-key-theme '(navigation insert shift todo heading))
  
  ;; Custom re-binds for your specific preference
  (general-define-key
   :states 'normal
   :keymaps 'org-mode-map
   ">>" 'org-metaright
   "<<" 'org-metaleft))

;; Table of contents in Org Mode
(use-package toc-org
  :straight t
  :hook (org-mode . toc-org-mode))


(provide 'languages)









