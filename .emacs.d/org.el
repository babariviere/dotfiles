(defun open-notes ()
  "Open notes files quickly."
  (interactive)
  (find-file org-default-notes-file))

(use-package org
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
  (leader-define-key
   :states 'normal
   :keymaps 'org-mode-map
   "t" 'org-todo
   "s" 'org-schedule
   "d" 'org-deadline
   "f" 'org-open-at-point
   "S-h" 'org-shiftleft
   "S-l" 'org-shiftright)
  (leader-define-key
    :states 'normal
    "ol" 'org-store-link
    "oa" 'org-agenda
    "oc" 'org-capture
    "on" 'open-notes)
  (setq org-log-done t)
  (setq org-src-fontify-natively t)
  (setq org-src-window-setup 'current-window)

  (setq org-directory (expand-file-name "~/org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (list org-directory))

  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Keywords
  (setq org-todo-keywords
	'(
	  (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
	  (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
	  ))
  ;; use :list-colors-display
  (setq org-todo-keyword-faces
	'(("IDEA" . (:foreground "GoldenRod" :weight bold))
	  ("NEXT" . (:foreground "IndianRed1" :weight bold))
	  ("STARTED" . (:foreground "OrangeRed" :weight bold))
	  ("WAITING" . (:foreground "coral" :weight bold))
	  ("CANCELED" . (:foreground "sea green" :weight bold))
	  ("DELEGATED" . (:foreground "sea green" :weight bold))
	  ("SOMEDAY" . (:foreground "sea green" :weight bold))
	  ))

  ;; Capture templates
  (setq org-capture-templates
	'(("t"
	   "TODO"
	   entry
	   (file+headline org-default-notes-file "Tasks")
	   "* TODO %?\n  Added: %U\n  From: %a" :prepend t :kill-buffer t)
	  ))
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

;; store credentials in ~/.authinfo
(use-package org-jira
  :after org
  :config
  (setq jiralib-url (concat "https://" jira-site ".atlassian.net")))
