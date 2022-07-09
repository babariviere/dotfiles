;; TODO: use consult-apropos for K

(require 'general)
(require 'use-package)

(add-hook 'emacs-lisp-mode #'highlight-quoted-mode)

;; (general-def 'normal emacs-lisp-mode-map
;;   "K" #'describe-symbol)

(amber/leader-keys emacs-lisp-mode-map
  "C-m" '(macrostep-expand :wk "expand macro"))

(use-package eros
  :hook (emacs-lisp-mode . eros-mode)
  :general
  (amber/leader-keys emacs-lisp-mode-map
    "C-e" '(:ignore t :wk "eval")
    "C-e C-b" '(eval-buffer :wk "eval buffer")
    "C-e C-e" '(eros-eval-last-sexp :wk "eval expr")
    "C-e C-d" '(eros-eval-defun :wk "eval defun")
    "C-e C-r" '(eval-region :wk "eval region")))

(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(provide 'amber-elisp)
