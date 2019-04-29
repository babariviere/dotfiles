(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable t
	lsp-ui-flycheck-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))
