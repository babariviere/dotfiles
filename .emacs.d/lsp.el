(use-package lsp-mode
  :commands lsp
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable t
	lsp-ui-flycheck-enable t
	lsp-ui-doc-header t
	lsp-ui-doc-include-signature t
	lsp-ui-doc-use-childframe nil)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'lsp-ui-doc-frame-hook
	    (lambda (frame _w)
	      (set-face-attribute 'default frame :font "Iosevka Term-10"))))

(use-package company-lsp
  :config
  (push 'company-lsp company-backends))
