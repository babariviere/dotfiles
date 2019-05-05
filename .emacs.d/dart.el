(use-package dart-mode
  :init
  (setq dart-format-on-save t)
  :config
  (add-hook 'dart-mode-hook 'lsp))
