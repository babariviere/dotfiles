(require 'use-package)

(use-package projectile
  :after general
  :config (projectile-mode 1)
  :custom
  (projectile-completion-system 'auto)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  :general
  (amber/leader-keys
   "SPC" '(projectile-find-file :which-key "find file")
   "p" '(:ignore t :which-key "project")
   "pp" '(projectile-switch-project :wk "switch project")))

(provide 'amber-project)
