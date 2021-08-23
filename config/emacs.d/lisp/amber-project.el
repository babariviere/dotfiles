(require 'use-package)

(use-package projectile
  :after general
  :config (projectile-mode 1)
  :custom
  (projectile-completion-system 'auto)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  (projectile-project-search-path '("~/src" "~/src/github.com/babariviere"))
  (projectile-project-root-files-functions '(projectile-root-local
					     projectile-root-top-down
					     projectile-root-bottom-up
					     projectile-root-top-down-recurring))
  :general
  (amber/leader-keys
    "SPC" '(projectile-find-file :which-key "find file in project")
    "p" '(:ignore t :which-key "project")
    "pp" '(projectile-switch-project :wk "switch project")))

(provide 'amber-project)
