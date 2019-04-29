(use-package projectile
  :ensure t
  :after (evil)
  :config
  (projectile-mode +1)
  (define-key leader-map "p" 'projectile-command-map)
  (setq projectile-completion-system 'ivy))
