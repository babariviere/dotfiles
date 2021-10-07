;;; amber-common-lisp.el --- Common Lisp support for Emacs -*- lexical-binding: t -*-

;; Author:
;; Maintainer:
;; Version: version
;; Package-Requires: (sly sly-repl-ansi-color)


;; This file is not part of GNU Emacs
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Add support for Common Lisp to amber Emacs.

;;; Code:

(require 'use-package)

(defun amber/common-lisp-eval (form)
  "Eval FORM and print it with eros."
  (cl-destructuring-bind (output value) (sly-eval `(slynk:eval-and-grab-output ,form))
    (eros--make-result-overlay (concat output value)
      :where (point)
      :duration eros-eval-result-duration)))

(defun amber/common-lisp-sly-eval-last-sexp ()
  "Eval last lisp expression and print it with eros."
  (interactive)
  (amber/common-lisp-eval (sly-last-expression)))

(defun amber/common-lisp-sly-eval-defun ()
  "Eval last lisp expression and print it with eros."
  (interactive)
  (let ((form (apply #'buffer-substring-no-properties
                     (sly-region-for-defun-at-point))))
    (cond ((string-match "^(defvar " form)
           (sly-re-evaluate-defvar form))
          (t
           (amber/common-lisp-eval form)))))

(add-to-list 'auto-mode-alist '("/sbclrc\\'" . common-lisp-mode))

(use-package sly
  :custom
  (inferior-lisp-program "sbcl --noinform --no-linedit")
  :general
  (amber/local-leader-keys lisp-mode-map
	"" nil
	"'" 'sly
	"m" 'macrostep-expand
	"c" '(:ignore t :wk "compile")
	"cc" '(sly-compile-file :wk "compile file")
	"cC" '(sly-compile-and-load-file :wk "compile+load file")
	"cd" '(sly-compile-defun :wk "compile defun")
	"cl" '(sly-load-file :wk "load file")
	"cr" '(sly-compile-region :wk "compile region")
	"e" '(:ignore t :wk "eval")
	"eb" '(sly-eval-buffer :wk "eval buffer")
	"ee" '(amber/common-lisp-sly-eval-last-sexp :wk "eval last")
	"ed" '(amber/common-lisp-sly-eval-defun :wk "eval defun")
	"eD" '(sly-undefine-function :wk "undef function")
	"er" '(sly-eval-region :wk "eval region")
	"r" '(:ignore t :wk "repl")
	"rc" '(sly-mrepl-clear-repl :wk "clear repl")
	"rq" '(sly-quit-lisp :wk "quit connection")
	"rr" '(sly-restart-inferior-lisp :wk "restart connection")
	"rs" '(sly-mrepl-sync :wk "sync repl")
	"t" '(:ignore t :wk "trace")
	"tt" '(sly-toggle-trace-fdefinition :wk "trace def")
	"tu" '(sly-untrace-all :wk "untrace all")))

(use-package sly-repl-ansi-color
  :defer t
  :init
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(provide 'amber-common-lisp)

;;; amber-common-lisp.el ends here
