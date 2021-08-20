(require 'use-package)

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package consult-lsp
  :general
  (lsp-mode-map
   [remap xref-find-apropos] #'consult-lsp-symbols))

(provide 'amber-lsp)
