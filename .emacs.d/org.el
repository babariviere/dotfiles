(use-package org
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (leader-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "t" 'org-todo)
  (leader-define-key
    :states 'normal
    "ol" 'org-store-link
    "oa" 'org-agenda
    "oc" 'org-capture)
  (setq org-log-done t)
  (setq org-todo-keywords
	'((sequence "TODO" "WORKING" "REVIEW" "DONE")))
  (setq org-src-fontify-natively t)
  (setq org-src-window-setup 'current-window)
  ;;(setq org-todo-keyword-faces
  ;;	'(("TODO" . "blue")
  ;;	  ("WORKING" . "yellow")
  ;;	  ("REVIEW" . "red")
  ;;	  ("DONE" . "green")))
  )

(use-package evil-org
  :after (org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-bullets
  :after org
  :config
  (setq org-bullets-bullet-list '("∙"))
  (add-hook 'org-mode-hook 'org-bullets-mode))
