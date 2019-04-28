(use-package company
  :ensure t
  :bind (("C-SPC". company-complete))
  :config
  (global-company-mode)
  (setq company-require-match nil
	company-idle-delay .3
	company-echo-delay 0
	company-begin-commands '(self-insert-command))
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous))
