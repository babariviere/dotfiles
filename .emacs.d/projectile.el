(use-package projectile
  :after (evil)
  :config
  (projectile-mode +1)
  (leader-define-key
    :states 'normal
    "p" 'projectile-command-map)
  (setq projectile-generic-command "fd . -0 -c never --ignore-file .gitignore 2>/dev/null")
  (setq projectile-completion-system 'ivy)

  ;; add project root files
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package counsel-projectile
  :config
  (add-hook 'after-init-hook 'counsel-projectile-mode))
