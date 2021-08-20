(require 'use-package)

(use-package elixir-mode
  :hook ((elixir-mode . lsp)))

(use-package alchemist
  :hook (elixir-mode . alchemist-mode)
  :general
  ('normal alchemist-mode-map
           ;; let lsp handle documentation
           "K" nil))

(provide 'amber-elixir)
