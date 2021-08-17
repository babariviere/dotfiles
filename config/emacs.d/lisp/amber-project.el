(require 'use-package)

(use-package projectile
  :after general
  :config (projectile-mode 1)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action #'projectile-dired)
  :general
  (amber/leader-keys
   "SPC" '(projectile-find-file :which-key "find file")
   "p" '(:keymap projectile-command-map :which-key "project")))

(use-package counsel-projectile
  :config (counsel-projectile-mode 1))

(provide 'amber-project)
