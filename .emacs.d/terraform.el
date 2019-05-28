(use-package terraform-mode
  :after lsp-mode
  :init
  ;; (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  ;; (add-hook 'terraform-mode-hook #'lsp)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "terraform-lsp")
					:major-modes '(terraform-mode)
					:server-id 'terraform-lsp)))

(use-package company-terraform
  :after terraform-mode
  :config
  (company-terraform-init))
