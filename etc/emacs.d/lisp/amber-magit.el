(require 'use-package)

(use-package magit
  :general
  (amber/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "git status")))

(use-package forge
  :after magit)

(provide 'amber-magit)
