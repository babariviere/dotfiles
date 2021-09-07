(require 'use-package)

(defun amber/git-commit-in-insert-state-h ()
  "Start git-commit-mode in insert state only if buffer is blank."
  (when (and (bound-and-true-p evil-mode) (bobp) (eolp))
    (evil-insert-state)))

(use-package magit
  :hook (git-commit-setup . amber/git-commit-in-insert-state-h)
  :general
  (amber/leader-keys
    "g" '(:ignore t :wk "git")
    "gg" '(magit-status :wk "git status")))

(use-package evil-collection-magit
  :after magit)

(use-package forge
  :after magit)

(use-package magit-todos
  :after magit)

(provide 'amber-magit)
