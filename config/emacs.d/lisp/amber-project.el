(require 'use-package)

(defun amber/project-ignored-p (project-root)
  "Return non-nil if PROJECT-ROOT must be ignored from project list."
  (let ((filename (f-filename project-root)))
    (equal filename ".git")))

(use-package projectile
  :after general
  :config
  (delete "default.nix" projectile-project-root-files)
  (projectile-mode 1)
  :custom
  (projectile-completion-system 'auto)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-globally-ignored-files '(".DS_Store" "TAGS"))
  ;; Run `projectile-discover-projects-in-search-path` manually instead.
  (projectile-auto-discover nil)
  (projectile-track-known-projects-automatically nil)
  (projectile-project-search-path '(("~/src" . 3)))
  (projectile-project-root-files-functions '(projectile-root-local
											 projectile-root-top-down
											 projectile-root-bottom-up
											 projectile-root-top-down-recurring))
  (projectile-ignored-project-function #'amber/project-ignored-p)
  :general
  (amber/leader-keys
    "SPC" '(projectile-find-file :which-key "find file in project")
    "p" '(:ignore t :which-key "project")
    "pi" '(projectile-invalidate-cache :wk "invalidate cache")
    "pp" '(projectile-switch-project :wk "switch project")))

(provide 'amber-project)
