(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp)
  :init
  (setq rust-format-on-save t))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :config
  (leader-define-key
    :states 'normal
    :keymaps 'rust-mode-map
    "c e" #'cargo-process-bench
    "c b" #'cargo-process-build
    "c a" #'cargo-process-add
    "c r" #'cargo-process-rm
    "c t" #'cargo-process-test
    "c r" #'cargo-process-run
    "c d" #'cargo-process-doc-open
    "c c" #'cargo-process-clean))

;; (use-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
