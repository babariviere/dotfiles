(require 'use-package)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))

(provide 'amber-dired)
