(require 'use-package)

(use-package magit
  :general
  (amber/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "git status")))

(use-package forge
  :after magit)

(use-package magit-todos
  :after magit)

(provide 'amber-magit)
