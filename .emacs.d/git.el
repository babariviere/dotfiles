(use-package magit
  :ensure t
  :config
  (leader-define-key
   :states 'normal
   "g s" 'magit-status)
  )

(use-package evil-magit
  :ensure t
  :after magit)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)
