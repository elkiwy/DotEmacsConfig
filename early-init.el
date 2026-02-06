

;;UI Cleanup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;;Disable package.el since we use straight.el
(setq package-enable-at-startup nil)
