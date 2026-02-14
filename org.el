;;; languages.el --- Language specific configurations -*- lexical-binding: t; -*-

;;-----------------------------------------------------------------------------
;; Org Mode
(use-package org
  :hook (org-mode . org-indent-mode)
  :config
  (setq org-hide-leading-stars t) ; Hides all but one star for a cleaner look
  (setq org-adapt-indentation nil) ; Ensures Org doesn't add real spaces

  ;; Todo Keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p)" "|" "DONE(d)" )))

  ;; Tags
  (setq org-tag-alist
        '(
          (:startgroup) ("easy" . ?e) ("medium" . ?m) ("hard" . ?h) (:endgroup)
          ("beardedbear" . ?b) ("casa" . ?c) ("work" . ?w) ("other" . ?o) 
         ))
  (setq org-tag-faces
        '(("easy"         . (:foreground "#98be65" :weight bold))
          ("medium"       . (:foreground "#ECBE7B" :weight bold))
          ("hard"         . (:foreground "#ff6c6b" :weight bold))
          ("beardedbear"  . (:foreground "#da8548" :weight bold))
          ("casa"         . (:foreground "#4db5bd" :weight bold))
          ("work"         . (:foreground "#ECBE7B" :weight bold))
          ("other"        . (:foreground "#9ca0a4" :weight bold))
          ))


  ;; Colors (Faces)
  (setq org-todo-keyword-faces
        '(("TODO"        . (:foreground "#f0b0b0" :weight bold))
          ("IN-PROGRESS" . (:foreground "#51afef" :weight bold))
          ("DONE"        . (:foreground "#5D8D53" :weight bold))
          ))

  ;; Dates
  (set-face-attribute 'org-date nil :foreground "#c698dd" :underline nil :weight 'normal)
  (set-face-attribute 'org-date nil :foreground "#c0c0c0" :underline nil :weight 'normal)

  ;; Priorities
  (setq org-priority-faces
        '((?A . (:foreground "#ff6c6b" :weight bold))
          (?B . (:foreground "#ECBE7B" :weight bold))
          (?C . (:foreground "#73797e" :weight bold)))))


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

(defun my/org-insert-daily-header ()
  "Insert a 2nd level Org header with a shiftable date."
  (interactive)
  (insert "** ")
  (org-insert-time-stamp (current-time) nil t)
  (insert "\n"))

(defun my/highlight-current-date ()
  (interactive)
  (highlight-phrase (format-time-string "\\[.* Sat\\]" (current-time)) 'nerd-icons-lred)
  (highlight-phrase (format-time-string "\\[.* Sun\\]" (current-time)) 'nerd-icons-lred)
  (highlight-phrase (format-time-string "\\[%Y-%m-%d %a\\]" (current-time)) 'magit-branch-remote-head))





;;-----------------------------------------------------------------------------
;; Org Roam

(use-package org-roam
  :ensure t
  :demand t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-directory "/Users/stefanobertoli/Library/Mobile Documents/com~apple~CloudDocs/OrgRoam")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
   '(
     ("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)

     ("g" "game" plain
      (file "/Users/stefanobertoli/Library/Mobile Documents/com~apple~CloudDocs/OrgRoam/Templates/GameNoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-game-${slug}.org"
                         "#+title: ${title}\n#+filetags: game\n#+date: %U\n")
      :unnarrowed t)

     ("w" "work" plain
      (file "/Users/stefanobertoli/Library/Mobile Documents/com~apple~CloudDocs/OrgRoam/Templates/WorkNoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-work-${slug}.org"
                         "#+title: ${title}\n#+filetags: work\n#+date: %U\n")
      :unnarrowed t)

     ("p" "personal" plain
      (file "/Users/stefanobertoli/Library/Mobile Documents/com~apple~CloudDocs/OrgRoam/Templates/PersonalNoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-personal-${slug}.org"
                         "#+title: ${title}\n#+filetags: personal\n#+date: %U\n")
      :unnarrowed t)

     ("b" "beardedbear" plain
      (file "/Users/stefanobertoli/Library/Mobile Documents/com~apple~CloudDocs/OrgRoam/Templates/BeardedBearNoteTemplate.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-beardedbear-${slug}.org"
                         "#+title: ${title}\n#+filetags: beardedbear\n#+date: %U\n")
      :unnarrowed t)

     ;;("t" "Task" plain "%?"
     ;; :if-new (file+head "tasks/%<%Y%m%d%H%M%S>-${slug}.org" 
     ;;                    "#+title: ${title}\n#+filetags: :task:\n\n[[id:%(alist-get (completing-read \"Project: \" my/org-roam-project-list) my/org-roam-project-list nil nil 'equal)][Back to Project]]\n")
     ;; :immediate-finish t
     ;; :after-finalize my/org-roam-link-task-to-sbs)
     ;; ;;:after-finalize my/org-roam-link-task-to-project)

     ("t" "Task" plain
      "%?"
      :if-new (file+head "tasks/%<%Y%m%d%H%M%S>-${slug}.org" 
                         "#+title: ${title}\n#+filetags: :task:\n\n%(my/org-roam-project-link-string)\n")
      :unnarrowed t
      :after-finalize my/org-roam-link-task-to-project)

     )
    )
  :config
  (org-roam-setup)
  )
  


(setq org-agenda-files (list org-roam-directory))
(setq org-agenda-span 21)
(setq org-agenda-start-on-weekday 1) ; 1 starts the view on Monday; nil starts on today



(setq org-startup-folded t)


(setq my/org-roam-project-list
      '(
        ("Personal" . "D276618E-9375-474C-931D-CEF53987C9AF")
        ("SBS"      . "E3FC5D5A-1F4F-4022-AEB2-A88F7E68D90E")
        ))



(defvar my/org-roam-last-project-id nil
  "Stores the ID of the last selected project during capture.")

(defun my/org-roam-project-link-string ()
  "Prompt for a project and return a link string. Stores choice for the linking hook."
  (let* ((project-name (completing-read "Link to Project: " my/org-roam-project-list))
         (project-id (alist-get project-name my/org-roam-project-list nil nil 'equal)))
    (setq my/org-roam-last-project-id project-id)
    (format "[[id:%s][Back to %s Project]]" project-id project-name)))

(defun my/org-roam-link-task-to-project ()
  "Append the link to the task into the selected project note's 'Tasks' section."
  (let (task-title task-id)
    ;; Switch to the captured note's buffer to get its data
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (save-excursion
        (goto-char (point-min))
        ;; Get title from #+title keyword
        (setq task-title (or (cadr (assoc "TITLE" (org-collect-keywords '("TITLE")))) "No Title"))
        ;; Get ID from the first property drawer in the file
        (setq task-id (org-id-get (point-min) t))))

    (let* ((project-location (org-id-find my/org-roam-last-project-id t)))

    (if (and task-id project-location)
        (progn
          (with-current-buffer (marker-buffer project-location)
            (save-excursion
              (goto-char (point-min))
              ;; Search for * Tasks header
              (if (re-search-forward "^\\* Tasks" nil t)
                  (org-end-of-subtree t t)
                (goto-char (point-max)))
              
              ;; Ensure newline and insert link
              (unless (bolp) (insert "\n"))
              (insert (format "** TODO [[id:%s][%s]]\n" task-id task-title))
              (save-buffer)
              (message "SUCCESS: Task link added to project."))))
      (message "ERROR: Missing Task ID or Project File not found!")))))

(provide 'org)
