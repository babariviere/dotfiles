;;; amber-go.el --- Go support -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Version: version
;; Package-Requires: (go-mode go-eldoc)
;; Homepage: homepage
;; Keywords: keywords


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

;; Add Go support to amber Emacs.

;;; Code:

(require 'use-package)

(defvar amber/go-last-test nil
  "The last test run.")

(defun amber/go--spawn (cmd)
  "Spawn go CMD and keep selected window."
  (save-selected-window
    (compile cmd)))

(defun amber/go--run-tests (args)
  "Run test.  Execute cmd go test ARGS."
  (let ((cmd (concat "go test " args)))
    (setq amber/go-last-test (concat "cd " default-directory ";" cmd))
    (amber/go--spawn cmd)))

(defun amber/go-test-rerun ()
  "Rerun last test."
  (interactive)
  (if amber/go-last-test
      (amber/go--spawn amber/go-last-test)
    (amber/go-test-all)))

(defun amber/go-test-all ()
  "Run all test."
  (interactive)
  (amber/go-run-tests ""))

(defun amber/go-test-nested ()
  "Run all nested test."
  (interactive)
  (amber/go-run-tests "./..."))

(defun amber/go-test-single ()
  "Run single test."
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (+go--run-tests (concat "-run" "='" (match-string-no-properties 2) "'")))
    (error "Must be in a _test.go file")))

(defun amber/go-toggle-test-file-name ()
  "Return the test/source file name."
  (let ((file-name (file-name-base (buffer-file-name))))
    (if (s-suffix-p "_test" file-name)
	(concat (string-remove-suffix "_test" file-name) ".go")
      (concat file-name "_test.go"))))

;; TODO: this function is not ideal, need some rework:
;; - split it in two functions (look at exunit)
;; - add support for project root tests folder
(defun amber/go-toggle-file-and-test ()
  (interactive)
  (let ((file-name (amber/go-toggle-test-file-name)))
    (if (file-exists-p file-name)
	(find-file file-name)
      (error "No file found."))))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp)
  :general
  (amber/local-leader-keys go-mode-map
    "t" '(:ignore t :wk "test")
    "ts" '(amber/go-test-single :wk "test signle")
    "ta" '(amber/go-test-all :wk "test all")
    "tr" '(amber/go-test-rerun :wk "rerun test")
    "tn" '(amber/go-test-nested :wk "test nested")))

(use-package go-gen-test
  :general
  (amber/local-leader-keys go-mode-map
    "tg" '(go-gen-test-dwim :wk "generate tests")
    "tG" '(go-gen-test-all :wk "generate all tests")))

(provide 'amber-go)

;;; amber-go.el ends here
