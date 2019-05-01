(use-package org
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (leader-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "t" 'org-todo)
  (setq org-log-done t)
  (setq org-todo-keywords
	'((sequence "TODO" "WORKING" "REVIEW" "DONE")))
  ;;(setq org-todo-keyword-faces
  ;;	'(("TODO" . "blue")
  ;;	  ("WORKING" . "yellow")
  ;;	  ("REVIEW" . "red")
  ;;	  ("DONE" . "green")))
  )

(use-package evil-org
  :after (org evil)
  :ensure t
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :ensure t
  :after org
  :config
  (setq org-bullets-bullet-list '("∙"))
  (add-hook 'org-mode-hook 'org-bullets-mode))
