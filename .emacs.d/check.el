(use-package flycheck
  :init
  (global-flycheck-mode t))

(use-package fic-mode
  :hook (prog-mode . (lambda () (fic-mode 1)))
  :config
  (set-face-attribute 'fic-face nil :background nil :foreground "blue violet" :weight 'bold))
