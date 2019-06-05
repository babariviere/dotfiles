(defun indent-buffer ()
  (interactive)
  (save-excursion
	(indent-region (point-min) (point-max) nil)))


(define-minor-mode terraform-format-on-save-fix-mode
  "Format file on save + apply indent (because breaked in 0.12)"
  :lighter ""
  (if terraform-format-on-save-fix-mode
	  (add-hook 'before-save-hook #'terraform-format-fix nil t)
	(remove-hook 'before-save-hook #'terraform-format-fix t)))

(defun terraform-format-fix ()
  (terraform-format-buffer)
  (indent-buffer))

(use-package terraform-mode
  :after lsp-mode
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-fix-mode)
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
