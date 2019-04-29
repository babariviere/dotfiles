(use-package org
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (evil-define-key 'normal org-mode-map (kbd (leader "o t")) 'org-todo)
  (setq org-log-done t))

(use-package evil-org
  :after org
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
