(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package company-terraform
  :after terraform-mode
  :config
  (company-terraform-init))
