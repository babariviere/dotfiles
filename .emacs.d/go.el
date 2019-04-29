(use-package go-mode
  :ensure t
  :init (add-hook 'go-mode-hook
		  (lambda ()
		    (setq gofmt-command "goimports")
		    (add-hook 'before-save-hook 'gofmt-before-save)
		    (setq truncate-lines t)
		    (setq indent-tabs-mode t)
		    (setq tab-width 4)))
  (add-hook 'go-mode-hook #'lsp))

;;(use-package company-go
;;  :ensure t
;;  :config
;;  (push 'company-go company-backends))
