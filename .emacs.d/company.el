(use-package company
  :ensure t
  :bind (("C-SPC". company-complete))
  :config
  (global-company-mode)
  (setq company-require-match nil
	company-idle-delay .3
	company-echo-delay 0
	company-begin-commands '(self-insert-command))
  ;; <return> is for windowed Emacs; RET is for terminal Emacs
  (dolist (key '("<return>" "RET"))
    ;; Here we are using an advanced feature of define-key that lets
    ;; us pass an "extended menu item" instead of an interactive
    ;; function. Doing this allows RET to regain its usual
    ;; functionality when the user has not explicitly interacted with
    ;; Company.
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
		  :filter ,(lambda (cmd)
			     (when (company-explicit-action-p)
			       cmd)))))
  (general-define-key
   :keymaps 'company-active-map
   "<tab>" #'company-complete-selection
   "TAB" #'company-complete-selection
   "SPC" nil
   "C-j" #'company-select-next
   "C-k" #'company-select-previous)

  (setq company-auto-complete-chars nil))
