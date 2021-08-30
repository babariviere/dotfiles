(require 'use-package)

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
         (fennel-mode . lispy-mode)))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :custom
  (lispyville-key-theme
   '((operators normal)
     c-w
     (prettify insert)
     (atom-movement t)
     slurp/barf-lispy
     additional
     additional-insert))
  :config
  (lispyville-set-key-theme))

(provide 'amber-lisp)
