(defun go-custom-mode-hook ()
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq truncate-lines t)
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (evil-define-key 'normal go-mode-map (kbd (leader "g a")) 'go-add-tags))

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

