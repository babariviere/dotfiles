(require 'use-package)
(require 'diminish)

(use-package lispy
  :hook ((lisp-mode . lispy-mode)
         (emacs-lisp-mode . lispy-mode)
	     (lisp-data-mode . lispy-mode)
         (ielm-mode . lispy-mode)
         (scheme-mode . lispy-mode)
         (racket-mode . lispy-mode)
         (hy-mode . lispy-mode)
         (lfe-mode . lispy-mode)
         (dune-mode . lispy-mode)
         (clojure-mode . lispy-mode)
         (fennel-mode . lispy-mode))
  :config
  (setq lispy-colon-p nil)
  (add-hook 'lispy-mode-hook #'turn-off-smartparens-mode)
  (diminish 'lispy-mode))

(dolist (f '(;; Nyxt
             define-key define-mode
             define-configuration define-command
             define-command-global

             ;; Stumpwm
             defcommand))
  (put f 'lisp-indent-function 'defun))

(provide 'amber-lisp)
