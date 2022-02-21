;;; amber-company.el --- Company -*- lexical-binding: t -*-

;; Author:
;; Maintainer:
;; Version: version
;; Package-Requires: (dependencies)
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

;; Add company support to Amber Emacs.

;;; Code:

(require 'use-package)

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-backends '((company-capf :with company-yasnippet) company-files company-dabbrev-code company-dabbrev company-ispell))
  (company-auto-commit nil)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Only search the current buffer for `company-dabbrev' (a backend that
  ;; suggests text your open buffers). This prevents Company from causing
  ;; lag once you have a lot of buffers open.
  (company-dabbrev-other-buffers nil)
  ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
  ;; domain-specific words with particular casing.
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  :general
  (:keymaps 'company-active-map
			"C-j" #'company-select-next
			"C-k" #'company-select-previous
			;; tab is used by yasnippet
			"<tab>"  nil
			"TAB" nil
			"RET" #'company-complete-selection
			[C-return] #'evil-ret))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  ;; remove that ugly green color for yasnippet completion
  (setq company-box-backends-colors nil)
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0)))

(defvar amber/company-completion-styles '(basic partial-completion orderless)
  "Completion styles for company to use.")

(defun amber/company-capf--candidates-around (fn &rest args)
  "Highlight partial completion and change completion styles with company-capf.

FN and ARGS are the function and arguments of company-capf."
  (let ((orderless-match-faces [completions-common-part])
        (completion-styles amber/company-completion-styles))
    (apply fn args)))

(advice-add 'company-capf--candidates :around #'amber/company-capf--candidates-around)

(provide 'amber-company)

;;; amber-company.el ends here
