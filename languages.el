;;; languages.el --- Language specific configurations -*- lexical-binding: t; -*-


;;-----------------------------------------------------------------------------
;; Completion Engine
(use-package company-mode
  :config
  ;; Fix for evil-mode's "." command not working when text is autocompleted
  ;; This tells Evil to treat these company actions as part of the text insertion
  (with-eval-after-load 'evil
    (evil-declare-change-repeat 'company-complete)
    (evil-declare-change-repeat 'company-complete-selection)
    (evil-declare-change-repeat 'company-complete-common)
    (evil-declare-change-repeat 'company-complete-number)
    (evil-declare-ignore-repeat 'company-abort)
    (evil-declare-ignore-repeat 'company-cancel)
    (evil-declare-ignore-repeat 'company-select-next)
    (evil-declare-ignore-repeat 'company-select-previous)))
(add-hook 'after-init-hook 'global-company-mode)


;;-----------------------------------------------------------------------------
;; Treesit
(require 'treesit)
(setq treesit-language-source-alist
      '(
        (jai "https://github.com/constantitus/tree-sitter-jai")
        (odin "https://github.com/tree-sitter-grammars/tree-sitter-odin")
        (dockerfile "https://github.com/camertron/tree-sitter-dockerfile")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        ))

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
;; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))
(add-hook 'prog-mode-hook #'yas-minor-mode)


;;-----------------------------------------------------------------------------
;; LSP (Eglot)
(use-package lsp-mode
  :commands lsp

  :init
  (setq lsp-enable-snippet t)
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(jai-ts-mode . "jai")))

  :config
  (setq lsp-enable-indentation nil)    ; Stop lsp-mode from managing indentation
  (setq lsp-enable-on-type-formatting nil) ; Stop formatting as you type
  (setq lsp-headerline-breadcrumb-enable nil)


  (setq lsp-log-io t)
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection '("/Users/stefanobertoli/Documents/Jails/bin/jails" "-jai_path" "/Users/stefanobertoli/Documents/jai/bin/jai"))
                     :major-modes '(jai-ts-mode)
                     :server-id 'jails))
  
  ;; Add the hook to your major mode
  :hook ((jai-ts-mode . lsp)
         (jai-ts-mode . yas-minor-mode)
         (php-mode . lsp)
         (python-mode . lsp)
         (python-ts-mode . lsp)
         (dockerfile-mode . lsp)
         (dockerfile-ts-mode . lsp)
         (js-ts-mode . lsp)
         (typescript-ts-mode . lsp)
         (tsx-ts-mode . lsp)
         (html-mode . lsp)
         (css-mode . lsp)
         (json-ts-mode . lsp)))





;;-----------------------------------------------------------------------------
;; Jai
;;NOTE: I had to manually fix Jails code and build a custom executable to fix diagnostics problems
(setq compilation-error-regexp-alist '()) ;;Workaround to fix missing variables?
(setq compilation-error-regexp-alist-alist '())
(use-package jai-ts-mode
  :straight (jai-ts-mode :type git :host github :repo "cpoile/jai-ts-mode")
  :mode "\\.jai\\'")




;;-----------------------------------------------------------------------------
;; Odin
(use-package odin-ts-mode
  :straight (odin-ts-mode :type git :host github :repo "Sampie159/odin-ts-mode")
  :mode "\\.odin\\'")








;;-----------------------------------------------------------------------------
;; Python Support
(use-package python
  :mode ("\\.py\\'" . python-mode))


;;-----------------------------------------------------------------------------
;; PHP Support
(use-package php-mode
  :mode "\\.php\\'")




;;-----------------------------------------------------------------------------
;; GLSL Support
(use-package glsl-mode
  :mode "\\.hlsl\\'")


;;-----------------------------------------------------------------------------
;; Docker & YAML Support
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package docker-compose-mode
  :mode "docker-compose\\.yml\\'")















;;-----------------------------------------------------------------------------
;; JavaScript & TypeScript Support
(use-package js
  :mode ("\\.js\\'" . js-ts-mode))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;;-----------------------------------------------------------------------------
;; HTML & CSS Support
(use-package mhtml-mode
  :mode "\\.html\\'")

(use-package css-mode
  :mode "\\.css\\'")

;;-----------------------------------------------------------------------------
;; JSON Support
(use-package json-ts-mode
  :mode "\\.json\\'"
  :config
  (defun my/json-format ()
    "Pretty-format the JSON in the current buffer or region."
    (interactive)
    (if (use-region-p)
        (json-pretty-print (region-beginning) (region-end))
      (json-pretty-print-buffer)))
  
  ;; Map it to a local key if desired, but we'll also add it to the leader keys
  (general-define-key
   :keymaps 'json-ts-mode-map
   :states 'normal
   "gq" 'my/json-format))


(provide 'languages)









