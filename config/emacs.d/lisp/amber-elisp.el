;; TODO: use consult-apropos for K

(require 'general)
(require 'use-package)

(add-hook 'emacs-lisp-mode #'highlight-quoted-mode)

(general-def 'normal emacs-lisp-mode-map
  "K" #'describe-symbol)

(amber/local-leader-keys
  'normal emacs-lisp-mode-map
  "m" '(macrostep-expand :wk "expand macro"))

(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  :general
  (amber/local-leader-keys emacs-lisp-mode-map
    "e" '(:ignore t :wk "eval")
    "eb" '(eval-buffer :wk "eval buffer")
    "ee" '(eros-eval-last-sexp :wk "eval expr")
    "ed" '(eros-eval-defun :wk "eval defun")
    "er" '(eval-region :wk "eval region")))

(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(provide 'amber-elisp)
