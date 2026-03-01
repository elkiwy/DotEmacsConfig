

;;UI Cleanup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; Disable package.el since we use straight.el
(setq package-enable-at-startup nil)
(setq package--init-file-ensured t)
;; The existence of ~/.emacs.d/elpa can sometimes trigger package.el loading.
;; You may wish to delete that directory manually.
