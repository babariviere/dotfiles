(require 'use-package)

(use-package elixir-mode
  :hook ((elixir-mode . lsp)))

(use-package alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (with-eval-after-load 'evil-collection
    (general-def 'normal alchemist-mode-map
      "K" nil
      :package 'alchemist)))

(provide 'amber-elixir)
