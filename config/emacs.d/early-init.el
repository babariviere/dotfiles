;; Early Init configuration

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 4)         ; Give some breathing room

(menu-bar-mode -1)           ; Disable the menu bar
(show-paren-mode 1)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(column-number-mode)

(defalias 'yes-or-no-p #'y-or-n-p)

(setq inhibit-startup-screen t)

(let* ((cache-dir (or (getenv "XDG_CACHE_HOME")
                      (concat (getenv "HOME") "/.cache")))
       (auto-save-dir (concat cache-dir "/emacs/autosave"))
       (backup-dir (concat cache-dir "/emacs/backup")))
  (make-directory auto-save-dir t)
  (make-directory backup-dir t)
  (setq auto-save-list-file-prefix auto-save-dir
        auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(concat auto-save-dir "/tramp-\\2") t)
          (".*" ,auto-save-dir t))
        backup-directory-alist `(("." . ,backup-dir))))

(setq create-lockfiles nil)

(setq read-process-output-max (* 1024 1024))
(require 'gcmh)
(gcmh-mode 1)
