(use-package magit
  :config
  (leader-define-key
   :states 'normal
   "g s" 'magit-status)
  )

(use-package evil-magit
  :after magit)

(use-package git-gutter
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package ghub)
(use-package forge)
