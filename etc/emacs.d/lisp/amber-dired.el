(require 'use-package)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
  :disabled
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'amber-dired)
