(use-package projectile
  :ensure t
  :after (evil)
  :config
  (projectile-mode +1)
  (leader-define-key
    :states 'normal
    "p" 'projectile-command-map)
  (setq projectile-generic-command "fd . -0 -c never --ignore-file .gitignore")
  (setq projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (add-hook 'after-init-hook 'counsel-projectile-mode))
