(use-package terraform-mode)

(use-package company-terraform
  :after terraform-mode
  :config
  (company-terraform-init))
