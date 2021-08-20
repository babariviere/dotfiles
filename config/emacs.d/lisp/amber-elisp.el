;; TODO: use consult-apropos for K

(require 'general)
(require 'use-package)

(general-evil-define-key 'motion lisp-mode-shared-map "K"
                         (function describe-symbol))

(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  :general
  (amber/local-leader-keys lisp-mode-shared-map
                           "e" '(:ignore t :wk "eval")
                           "eb" '(eval-buffer :wk "eval buffer")
                           "ee" '(eros-eval-last-sexp :wk "eval expr")
                           "ed" '(eros-eval-defun :wk "eval defun")
                           "er" '(eval-region :wk "eval region")))

(provide 'amber-elisp)
