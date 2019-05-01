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
   "t a" #'go-add-tags)
  (leader-define-key
    :states 'normal
    :keymaps 'go-mode-map
    "t f" #'go-test-current-file
    "t t" #'go-test-current-test
    "t p" #'go-test-current-project
    "t c" #'go-test-current-coverage
    "b b" #'go-test-current-benchmark
    "b f" #'go-test-current-file-benchmarks
    "b p" #'go-test-current-project-benchmarks))

(use-package go-mode
  :hook (go-mode . lsp)
  :config
  (use-package godoctor)

  (use-package go-guru)

  (use-package gotest)

  (use-package go-add-tags)
  (add-hook 'go-mode-hook
	    #'go-custom-mode-hook))
