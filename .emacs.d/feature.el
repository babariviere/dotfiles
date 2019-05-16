(use-package feature-mode
  :init
  (setq feature-default-language "fi")
  :config
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))
