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

(defalias 'yes-or-no-p #'y-or-n-p)

(setq inhibit-startup-screen t)

(let* ((cache-dir (or (getenv "XDG_CACHE_HOME")
                      (concat (getenv "HOME") "/.cache")))
       (backup-dir (concat cache-dir "/emacs/backups")))
  (make-directory backup-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))))

(setq create-lockfiles nil)

(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)
