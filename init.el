
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
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(non-special-display . t))
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
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
  (set-face-attribute 'default nil :font "Fira Code" :height 120)

  ;;Enable line numbers
  (defun ab/enable-line-numbers () (interactive) (display-line-numbers-mode))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers)
  (add-hook 'prog-mode-hook #'hs-minor-mode)
  (setq-default display-line-numbers-width 4)

  ;;Enable recent files tracking
  (recentf-mode 1)
  (setq recentf-max-menu-items 250)
  (setq recentf-max-saved-items 250)

  ;; Disable audible bell
  (setq ring-bell-function 'ignore)

  ;; Scroll Margins
  (setq scroll-margin 5)             ; Keep a 3-line "buffer" at the top and bottom
  (setq scroll-conservatively 101)   ; If > 100, Emacs scrolls 1 line instead of jumping to center
  (setq scroll-step 1)               ; Keyboard scroll step is 1 line

  ;;Make it resizable to any precise size to avoid gaps
  (setq frame-resize-pixelwise t)
  (setq window-resize-pixelwise t)

  ;;Adjust the split divider lines width
  (setq window-divider-default-bottom-width 4)
  (setq window-divider-default-right-width 4) 
  (window-divider-mode 1)

  ;; Compilation and Terminal output at the bottom
  (setq compilation-scroll-output t)
  (add-to-list 'display-buffer-alist
               '("\\*compilation\\*"
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (window-height . 0.3)))
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))







;;-----------------------------------------------------------------------------
;; Font zoom
(defun my/global-zoom-in ()
  "Increase the global font size."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ old-face-attribute 10))))

(defun my/global-zoom-out ()
  "Decrease the global font size."
  (interactive)
  (let ((old-face-attribute (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- old-face-attribute 10))))

(defun my/global-zoom-reset ()
  "Reset the global font size to default."
  (interactive)
  (set-face-attribute 'default nil :height 120))

(global-set-key (kbd "s-=") 'my/global-zoom-in)
(global-set-key (kbd "s--") 'my/global-zoom-out)
(global-set-key (kbd "s-0") 'my/global-zoom-reset)

;;Avoid to rescale the window size when zooming
(setq frame-inhibit-implied-resize t)



;;-----------------------------------------------------------------------------
;; Setup Exec path (for executable binaries like LSPs)
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))



;;-----------------------------------------------------------------------------
;; Dired
(use-package dired
  :straight nil
  :hook
  (dired-mode . dired-hide-details-mode) ;;Hide all the extra informations by default, press "(" to toggle back on
  )

;Instead of deleting files permanently from dired, just move them to trash
(setq delete-by-moving-to-trash t
      trash-directory "/Users/stefanobertoli/.Trash/")



;;-----------------------------------------------------------------------------
;; Better mini-buffers searches

;; Rich annotations 
(use-package marginalia 
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; More fuzzy search
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) 

;; Remove "^" by default on 'counsel-M-x'
(setq ivy-initial-inputs-alist nil)



;;-----------------------------------------------------------------------------
;; Evil-Mode
(use-package evil
  :demand ; No lazy loading
  :init
  (setq evil-ex-search-highlight-all t)
  (setq evil-ex-search-persistent-highlight t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-symbol-word-search t)
  :config
  (evil-mode 1)
  (evil-select-search-module 'evil-search-module 'evil-search))





;;-----------------------------------------------------------------------------
;; Themes and Modeline
(use-package doom-themes
  :demand
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one t)
  (set-face-attribute 'default nil :background "#1d1e26")
  (set-face-attribute 'fringe nil :background "#1d1e26")
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package nerd-icons)
(use-package doom-modeline ;; Customization flags here : https://github.com/seagle0128/doom-modeline 
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (set-face-attribute 'mode-line nil :background "#15161e")
  (set-face-attribute 'mode-line-inactive nil :background "#191A22"))


(use-package rainbow-mode)

(global-hl-line-mode)
(set-face-attribute 'hl-line nil :background "#191A22" :extend t)


;;-----------------------------------------------------------------------------
;; Keybindings Helper

;;Tooltip helper window
(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.1)
  :config
  (which-key-mode))

;;Keybind definer
(use-package general
  :demand
  :config
  (general-evil-setup)

  (defun ab/run-project-command ()
    "Run a command from .emacs_commands in the project root."
    (interactive)
    (let* ((root (projectile-project-root))
           (commands-file (expand-file-name ".emacs_commands" root)))
      (if (file-exists-p commands-file)
          (let* ((commands (with-temp-buffer
                             (insert-file-contents commands-file)
                             (split-string (buffer-string) "\n" t)))
                 (command (completing-read "Run command: " commands)))
            (when (and command (not (string-empty-p command)))
              (let ((default-directory root))
                (pop-to-buffer (compile command)))))
        (message "No .emacs_commands file found in project root (%s)." root))))

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "." '(counsel-find-file :which-key "find file")
    ":" '(counsel-M-x :which-key "execute command")
    "/" '(counsel-projectile-rg :which-key "search project")
    "R" '(restart-emacs :which-key "restart emacs")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")
    "SPC" '(projectile-find-file :which-key "find file")

    ;; File
    "f" '(:ignore t :which-key "file")
    "ff" '(counsel-find-file :which-key "find file")
    "fr" '(counsel-recentf :which-key "recent files")
    "fs" '(save-buffer :which-key "save file")
    "fR" '(rg-menu :which-key "find")

    ;; Eval / EmacsLisp
    "e"  '(:ignore t :which-key "ELisp")
    "ee" '(eval-last-sexp :which-key "Evaluate last sexp")

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

    ;; Toggles
    "t" '(:ignore t :which-key "toggles")
    "tt" '(vterm-toggle :which-key "terminal")
    "tl" '(toggle-truncate-lines :which-key "truncate lines")

    ;; Projects
    "p" '(:ignore t :which-key "projects")
    "pp" '(projectile-switch-project :which-key "switch project")
    "pa" '(projectile-add-known-project :which-key "add project")
    "pr" '(projectile-remove-known-project :which-key "remove project")

    ;; Buffer
    "b" '(:ignore t :which-key "buffer")
    "bb" '(projectile-switch-to-buffer :which-key "switch buffer")
    "bm" 'buffer-menu
    "bd" 'kill-current-buffer

    ;; Hide/Show (Folding)
    "h" '(:ignore t :which-key "hideshow")
    "ha" '(hs-hide-all :which-key "hide all")
    "hl" '(hs-hide-level :which-key "hide level")
    "hh" '(hs-show-all :which-key "show all")
    "ht" '(hs-toggle-hiding :which-key "toggle folding")

    ;; Rest Client
    "r" '(:ignore t :which-key "RestClient")
    "rr" '(restclient-http-send-current :which-key "send request")
    "rb" '(restclient-http-send-current-raw :which-key "send request (raw)")

    ;; Org Mode
    "o"  '(:ignore t :which-key "org")
    "of" '(org-cycle :which-key "toggle fold")
    "oa" '(org-agenda :which-key "agenda")
    "ot" '(org-set-tags-command :which-key "set tags")
    "os" '(org-schedule :which-key "schedule")
    "od" '(my/org-insert-daily-header :which-key "insert current date")
    "oR" '(org-reload :which-key "Reload Org")

    ;; Org Roam Mode
    "or"  '(:ignore t :which-key "org roam")
    "orf"  '(org-roam-node-find :which-key "node find")
    "ori"  '(org-roam-node-insert :which-key "node insert")

    ))



;;-----------------------------------------------------------------------------
;; Smooth scrolling
(use-package good-scroll)
(good-scroll-mode 1)
(pixel-scroll-precision-mode 1)

(defun my/evil-scroll-down-smooth ()
  (interactive)
  (good-scroll-move (* -1 (/ (window-pixel-height) 2))))

(defun my/evil-scroll-up-smooth ()
  (interactive)
  (good-scroll-move (/ (window-pixel-height) 2)))

(general-def 'motion
  "C-d" 'my/evil-scroll-up-smooth
  "C-u" 'my/evil-scroll-down-smooth)



;;-----------------------------------------------------------------------------
;; Project Manager
(use-package projectile
  :demand
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
    "<escape>" #'transient-quit-one)
  :config
  (add-hook 'git-commit-setup-hook 'evil-insert-state))

(use-package evil-collection
  :after evil
  :demand
  :config
  (evil-collection-init))


(use-package diff-hl
  :straight t
  :config
  (add-hook 'after-save-hook 'diff-hl-update)
  (global-diff-hl-mode 1))
(global-diff-hl-mode)
(set-fringe-mode 3)



;;-----------------------------------------------------------------------------
;; Terminal Emulator
(use-package vterm)
(use-package vterm-toggle)

(add-to-list 'display-buffer-alist
      '("\\*vterm\\*"
         (display-buffer-in-side-window)
         (window-parameters . ((no-delete-other-windows . t)))
         (side . bottom)
         (slot . 0)
         (window-height . 0.3))) ; Adjust 0.3 to your preferred height (30%)

;; Ensure vterm-toggle knows to use the display-buffer logic
(setq vterm-toggle-fullscreen-p nil)
(setq vterm-toggle-scope 'project) 




;;-----------------------------------------------------------------------------
;; RipGrep Search
(use-package rg)



;;-----------------------------------------------------------------------------
;; Rest Client
(use-package restclient
  :straight t
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-jq :straight t))




;;-----------------------------------------------------------------------------
;; Language-Specific Configurations
(load (expand-file-name "languages.el" user-emacs-directory))
(load (expand-file-name "org.el" user-emacs-directory))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval my/highlight-current-date)
     (eval add-hook 'before-save-hook 'my/highlight-current-date nil t)
     (eval add-hook 'before-save-hook my/highlight-current-date nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
