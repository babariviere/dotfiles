(require 'use-package)

;; TODO: look at the bug fixes in this file
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/tools/lsp/%2Blsp.el
(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-enable-folding nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  ;; breaks sideline
  (lsp-eldoc-enable-hover nil)
  :general
  ('normal
   lsp-mode-map
   "gd" #'lsp-find-type-definition
   "K" #'lsp-describe-thing-at-point))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package consult-lsp
  :general
  (lsp-mode-map
   [remap xref-find-apropos] #'consult-lsp-symbols))

(provide 'amber-lsp)
