;;; amber-scheme.el --- Scheme support for Amber Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Bastien Riviere

;; Author: Bastien Riviere(defun amber/scheme-indent-function (indent-point state) <me@babariviere.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
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

;; Scheme support for Amber Emacs

;;; Code:

(require 'use-package)

(defun amber/scheme-indent-function (indent-point state)
  "Scheme mode function for the value of the variable `lisp-indent-function'.
This behaves like the function `lisp-indent-function', except that:

i) it checks for a non-nil value of the property `scheme-indent-function'
\(or the deprecated `scheme-indent-hook'), rather than `lisp-indent-function'.

ii) if that property specifies a function, it is called with three
arguments (not two), the third argument being the default (i.e., current)
indentation."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
										 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
										(progn (forward-sexp 1) (point))))
			method)
		(setq method (or (get (intern-soft function) 'scheme-indent-function)
						 (get (intern-soft function) 'scheme-indent-hook)))
		(cond ((or (eq method 'defun)
				   (and (null method)
						(> (length function) 3)
						(string-match "\\`def" function)))
			   (lisp-indent-defform state indent-point))
              ;; This next cond clause is the only change -mhw
			  ((and (null method)
                    (> (length function) 1)
										; The '#' in '#:' seems to get lost, not sure why
                    (string-match "\\`:" function))
               (let ((lisp-body-indent 1))
                 (lisp-indent-defform state indent-point)))
			  ((integerp method)
			   (lisp-indent-specform method state
									 indent-point normal-indent))
			  (method
			   (funcall method state indent-point normal-indent)))))))

(defun amber/scheme-set-indent ()
  (setq-local lisp-indent-function #'amber/scheme-indent-function))

(add-hook 'scheme-mode-hook #'amber/scheme-set-indent)

;; Guix functions
(put 'eval-when 'scheme-indent-function 1)
(put 'call-with-prompt 'scheme-indent-function 1)
(put 'test-assert 'scheme-indent-function 1)
(put 'test-assertm 'scheme-indent-function 1)
(put 'test-equalm 'scheme-indent-function 1)
(put 'test-equal 'scheme-indent-function 1)
(put 'test-eq 'scheme-indent-function 1)
(put 'call-with-input-string 'scheme-indent-function 1)
(put 'call-with-port 'scheme-indent-function 1)
(put 'guard 'scheme-indent-function 1)
(put 'lambda* 'scheme-indent-function 1)
(put 'substitute* 'scheme-indent-function 1)
(put 'match-record 'scheme-indent-function 2)

;; 'modify-inputs' and its keywords.
(put 'modify-inputs 'scheme-indent-function 1)
(put 'replace 'scheme-indent-function 1)

;; 'modify-phases' and its keywords.
(put 'modify-phases 'scheme-indent-function 1)
(put 'replace 'scheme-indent-function 1)
(put 'add-before 'scheme-indent-function 2)
(put 'add-after 'scheme-indent-function 2)

(put 'modify-services 'scheme-indent-function 1)
(put 'with-directory-excursion 'scheme-indent-function 1)
(put 'with-file-lock 'scheme-indent-function 1)
(put 'with-file-lock/no-wait 'scheme-indent-function 1)
(put 'with-profile-lock 'scheme-indent-function 1)
(put 'with-writable-file 'scheme-indent-function 2)

(put 'package 'scheme-indent-function 0)
(put 'package/inherit 'scheme-indent-function 1)
(put 'origin 'scheme-indent-function 0)
(put 'build-system 'scheme-indent-function 0)
(put 'bag 'scheme-indent-function 0)
(put 'gexp->derivation 'scheme-indent-function 1)
(put 'graft 'scheme-indent-function 0)
(put 'operating-system 'scheme-indent-function 0)
(put 'file-system 'scheme-indent-function 0)
(put 'manifest-entry 'scheme-indent-function 0)
(put 'manifest-pattern 'scheme-indent-function 0)
(put 'substitute-keyword-arguments 'scheme-indent-function 1)
(put 'with-store 'scheme-indent-function 1)
(put 'with-external-store 'scheme-indent-function 1)
(put 'with-error-handling 'scheme-indent-function 0)
(put 'with-mutex 'scheme-indent-function 1)
(put 'with-atomic-file-output 'scheme-indent-function 1)
(put 'call-with-compressed-output-port 'scheme-indent-function 2)
(put 'call-with-decompressed-port 'scheme-indent-function 2)
(put 'call-with-gzip-input-port 'scheme-indent-function 1)
(put 'call-with-gzip-output-port 'scheme-indent-function 1)
(put 'call-with-lzip-input-port 'scheme-indent-function 1)
(put 'call-with-lzip-output-port 'scheme-indent-function 1)
(put 'signature-case 'scheme-indent-function 1)
(put 'emacs-batch-eval 'scheme-indent-function 0)
(put 'emacs-batch-edit-file 'scheme-indent-function 1)
(put 'emacs-substitute-sexps 'scheme-indent-function 1)
(put 'emacs-substitute-variables 'scheme-indent-function 1)
(put 'with-derivation-narinfo 'scheme-indent-function 1)
(put 'with-derivation-substitute 'scheme-indent-function 2)
(put 'with-status-report 'scheme-indent-function 1)
(put 'with-status-verbosity 'scheme-indent-function 1)
(put 'with-build-handler 'scheme-indent-function 1)

(put 'mlambda 'scheme-indent-function 1)
(put 'mlambdaq 'scheme-indent-function 1)
(put 'syntax-parameterize 'scheme-indent-function 1)
(put 'with-monad 'scheme-indent-function 1)
(put 'mbegin 'scheme-indent-function 1)
(put 'mwhen 'scheme-indent-function 1)
(put 'munless 'scheme-indent-function 1)
(put 'mlet* 'scheme-indent-function 2)
(put 'mlet 'scheme-indent-function 2)
(put 'run-with-store 'scheme-indent-function 1)
(put 'run-with-state 'scheme-indent-function 1)
(put 'wrap-program 'scheme-indent-function 1)
(put 'with-imported-modules 'scheme-indent-function 1)
(put 'with-extensions 'scheme-indent-function 1)
(put 'with-parameters 'scheme-indent-function 1)
(put 'let-system 'scheme-indent-function 1)
(put 'with-build-variables 'scheme-indent-function 2)

(put 'with-database 'scheme-indent-function 2)
(put 'call-with-database 'scheme-indent-function 1)
(put 'call-with-transaction 'scheme-indent-function 1)
(put 'with-statement 'scheme-indent-function 3)
(put 'call-with-retrying-transaction 'scheme-indent-function 1)
(put 'call-with-savepoint 'scheme-indent-function 1)
(put 'call-with-retrying-savepoint 'scheme-indent-function 1)

(put 'call-with-container 'scheme-indent-function 1)
(put 'container-excursion 'scheme-indent-function 1)
(put 'eventually 'scheme-indent-function 1)

(put 'call-with-progress-reporter 'scheme-indent-function 1)
(put 'with-repository 'scheme-indent-function 2)
(put 'with-temporary-git-repository 'scheme-indent-function 2)
(put 'with-environment-variables 'scheme-indent-function 1)
(put 'with-fresh-gnupg-setup 'scheme-indent-function 1)

(put 'with-paginated-output-port 'scheme-indent-function 1)

(put 'with-shepherd-action 'scheme-indent-function 3)

(put 'with-http-server 'scheme-indent-function 1)

(require 'geiser)
(require 'geiser-mode)
(require 'geiser-guile)
(when (executable-find "guix")
  (add-to-list 'geiser-guile-load-path
               (expand-file-name "~/.config/guix/current/share/guile/site/3.0")))

(setq geiser-mode-company-p nil)
(advice-add 'geiser-completion--for-filename :override (lambda () nil))
(advice-add 'geiser-capf--for-filename :override (lambda () nil))

(use-package macrostep-geiser
  :after geiser-mode
  :hook (geiser-mode . macrostep-geiser-setup))

(provide 'amber-scheme)

;;; amber-scheme.el ends here
