
;;-----------------------------------------------------------------------------
;; Generic UI cleanup
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)


;;-----------------------------------------------------------------------------
;;Setup Straight.el Package Manager (from the Getting Started)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))




;;-----------------------------------------------------------------------------
;; Enable use-package (maybe deprecated since it should be on Emacs Core now.)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)



;;-----------------------------------------------------------------------------
;; Garbage Collector Magic Hack 
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))



;;-----------------------------------------------------------------------------
;;Setting defaults
(use-package emacs
  :init
  ;;Scratch buffer and init startup message
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message () (message "Welcome back Kiwy!"))

  ;;Enable only "y" or "n" for confirmations
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;;Set Everything use UTF-8
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;;Improve window title bar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)

  ;;Disable line wrapping by default
  (setq-default truncate-lines t)
  (setq-default truncate-partial-width-windows t)

  ;;Use Space and set tab-width
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;;Set MacOS keybindings
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier nil)
    (setq mac-control-modifier 'control))

  ;;Set Font
  (set-face-attribute 'default nil
    :font "Fira Code"
    :height 160)

  ;;Enable line numbers
  (defun ab/enable-line-numbers () (interactive) (display-line-numbers-mode))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers)
  (add-hook 'prog-mode-hook #'hs-minor-mode)

  ;;Enable recent files tracking
  (recentf-mode 1)
  (setq recentf-max-menu-items 250)
  (setq recentf-max-saved-items 250)

  ;; Disable audible bell
  (setq ring-bell-function 'ignore)
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))




;;-----------------------------------------------------------------------------
;; Setup Exec path (for executable binaries like LSPs)
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))



;;-----------------------------------------------------------------------------
;; Evil-Mode
(use-package evil
  :demand ; No lazy loading
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))



;;-----------------------------------------------------------------------------
;; Themes
(use-package doom-themes
  :demand
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one t)
  (set-face-attribute 'default nil :background "#1d1e26")
  (set-face-attribute 'fringe nil :background "#1d1e26")

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))



;;-----------------------------------------------------------------------------
;; Modeline
(use-package nerd-icons)
(use-package doom-modeline ;; Customization flags here : https://github.com/seagle0128/doom-modeline 
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (set-face-attribute 'mode-line nil :background "#15161e")
  (set-face-attribute 'mode-line-inactive nil :background "#1d1e26"))



;;-----------------------------------------------------------------------------
;; Keybindings Helper

;;Tooltip helper window
(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.25)
  :config
  (which-key-mode))

;;Keybind definer
(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "." '(counsel-find-file :which-key "find file")
    ":" '(counsel-M-x :which-key "execute command")
    "/" '(counsel-projectile-rg :which-key "search project")
    "r" '(restart-emacs :which-key "restart emacs")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")

    ;; File
    "f" '(:ignore t :which-key "file")
    "ff" '(counsel-find-file :which-key "find file")
    "fr" '(counsel-recentf :which-key "recent files")
    "fs" '(save-buffer :which-key "save file")

    ;; Code
    "c"  '(:ignore t :which-key "code")
    "cc" '(ab/run-project-command :which-key "run project command")
    "cd" '(xref-find-definitions :which-key "find definition")
    "cD" '(xref-find-references :which-key "find references")

    ;; Window
    "w" '(:ignore t :which-key "window")
    "wv" '(evil-window-vsplit :which-key "vertical split")
    "ws" '(evil-window-split :which-key "horizontal split")
    "wh" '(evil-window-left :which-key "window left")
    "wj" '(evil-window-down :which-key "window down")
    "wk" '(evil-window-up :which-key "window up")
    "wl" '(evil-window-right :which-key "window right")
    "wd" '(evil-window-delete :which-key "delete window")

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    ;; Don't show an error because SPC b ESC is undefined, just abort
    "b <escape>" '(keyboard-escape-quit :which-key t)
    "bd"  'kill-current-buffer

    ;; Hide/Show (Folding)
    "h" '(:ignore t :which-key "hideshow")
    "ha" '(hs-hide-all :which-key "hide all")
    "hh" '(hs-show-all :which-key "show all")
    "ht" '(hs-toggle-hiding :which-key "toggle folding")) )


;;-----------------------------------------------------------------------------
;; Project Manager
(use-package projectile
  :demand

  :general
  (leader-keys
    :states 'normal
    "SPC" '(projectile-find-file :which-key "find file")

    ;; Buffers
    "b b" '(projectile-switch-to-buffer :which-key "switch buffer")

    ;; Projects
    "p" '(:ignore t :which-key "projects")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "p p" '(projectile-switch-project :which-key "switch project")
    "p a" '(projectile-add-known-project :which-key "add project")
    "p r" '(projectile-remove-known-project :which-key "remove project"))
  :init
  (projectile-mode +1))





;;-----------------------------------------------------------------------------
;; Fuzzy Completion frontend
(use-package ivy
  :config
  (ivy-mode))

(use-package counsel
  :demand
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(use-package amx
  :demand
  :config
  (amx-mode 1))

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")



;;;;-----------------------------------------------------------------------------
;;;; Session Management (Workspace Restoration)
;;(use-package desktop
;;  :demand t
;;  :init
;;  (setq desktop-path (list user-emacs-directory)
;;        desktop-auto-save-timeout 30
;;        desktop-save t ;; Save without asking on exit
;;        desktop-load-locked-desktop t) ;; Open even if locked after a crash
;;  :config
;;  ;; Enable the mode to save state on exit
;;  (desktop-save-mode 1)
;;
;;  ;; Prompt to restore the last session on startup
;;  (add-hook 'after-init-hook
;;            (lambda ()
;;              (when (file-exists-p (expand-file-name ".emacs.desktop" user-emacs-directory))
;;                (if (y-or-n-p "Restore last session? ")
;;                    (desktop-read)
;;                  (desktop-clear))))))



;;-----------------------------------------------------------------------------
;; Git Client
(use-package magit
  :general
  (leader-keys
    "g" '(:ignore t :which-key "git")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "g g" '(magit-status :which-key "status")
    "g l" '(magit-log :which-key "log"))
  (general-nmap
    "<escape>" #'transient-quit-one))

(use-package evil-collection
  :after evil
  :demand
  :config
  (evil-collection-init))


(use-package diff-hl
  :init
  ;(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  ;(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))



;;-----------------------------------------------------------------------------
;; Terminal Emulator
(use-package vterm)
(use-package vterm-toggle
  :general
  (leader-keys
    "o t" '(vterm-toggle :which-key "terminal")))



;;;;-----------------------------------------------------------------------------
;; LSPs
(use-package company-mode)
(add-hook 'after-init-hook 'global-company-mode)



;;-----------------------------------------------------------------------------
;; Language-Specific Configurations
(load (expand-file-name "languages.el" user-emacs-directory))
