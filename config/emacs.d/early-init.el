;; Early Init configuration

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 4)         ; Give some breathing room

(menu-bar-mode -1)           ; Disable the menu bar

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(column-number-mode)

