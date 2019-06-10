(use-package lsp-mode
  :commands lsp
  :init
  (setenv "CGO_ENABLED" "0")
  (setq lsp-auto-guess-root t)
  :config
  (require 'lsp-clients)
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui
	:commands lsp-ui
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-sideline-enable t
        lsp-ui-peek-enable t
	lsp-ui-flycheck-enable t
	lsp-ui-doc-header t
	lsp-ui-doc-include-signature t
	lsp-ui-doc-use-childframe t
	lsp-ui-doc-max-width 100
	lsp-ui-doc-header nil)
  (set-face-attribute 'lsp-ui-doc-background nil :background (face-attribute 'default :background))
  (setq lsp-ui-doc-border (face-attribute 'default :foreground))
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'lsp-ui-doc-frame-hook
	    (lambda (frame _w)
	      (set-face-attribute 'default frame :font "Iosevka Term-10"))))

(use-package company-lsp
	:commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil
	company-lsp-async t
	company-lsp-cache-candidates 'auto
	company-lsp-enable-recompletion nil))

(use-package lsp-treemacs
	:commands lsp-treemacs-errors-list)
