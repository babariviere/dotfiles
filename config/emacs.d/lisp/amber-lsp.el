(require 'use-package)

;; TODO: look at the bug fixes in this file
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/tools/lsp/%2Blsp.el
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-enable-folding nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-enable nil)
  :general
  ('motion
   lsp-mode-map
   "K" #'lsp-describe-thing-at-point))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package consult-lsp
  :general
  (lsp-mode-map
   [remap xref-find-apropos] #'consult-lsp-symbols))

(provide 'amber-lsp)