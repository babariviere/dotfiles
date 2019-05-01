(defun go-custom-mode-hook ()
  "Create a custom hook for go mode."
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq truncate-lines t)
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (leader-define-key
   :states '(normal visual)
   :keymaps 'go-mode-map
   "t" 'go-add-tags))

(use-package go-mode
  :ensure t
  :hook (go-mode . lsp)
  :config
  (use-package godoctor
    :ensure t)

  (use-package go-guru
    :ensure t)

  (use-package go-add-tags
    :ensure t)
  (add-hook 'go-mode-hook
	    #'go-custom-mode-hook))
