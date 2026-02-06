
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

  ;;Use Space and set tab-width
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;;Set MacOS keybindings
		(when (eq system-type 'darwin)
				(setq mac-command-modifier 'super)
				(setq mac-option-modifier 'meta)
				(setq mac-control-modifier 'control))

		;;Set Font
		(set-face-attribute 'default nil
    :font "Fira Code"
    :height 160)

  ;;Enable line numbers
		(defun ab/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers)

)


;;-----------------------------------------------------------------------------
;; Evil-Mode
(use-package evil
  :demand ; No lazy loading
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

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))



;;-----------------------------------------------------------------------------
;; Modeline
(use-package nerd-icons)
(use-package doom-modeline ;; Customization flags here : https://github.com/seagle0128/doom-modeline 
  :ensure t
  :init (doom-modeline-mode 1))



;;-----------------------------------------------------------------------------
;; Keybindings Helper

;;Tooltip helper window
(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5) ; Open after .5s instead of 1s
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
    "x" '(execute-extended-command :which-key "execute command")
    "r" '(restart-emacs :which-key "restart emacs")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    ;; Don't show an error because SPC b ESC is undefined, just abort
    "b <escape>" '(keyboard-escape-quit :which-key t)
    "bd"  'kill-current-buffer ) )


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

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

