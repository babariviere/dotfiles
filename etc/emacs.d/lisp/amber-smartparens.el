;;; amber-smartparens.el --- Setup smartparens -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Package-Requires: (smartparens)


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

;; Setup smartparens for amber emacs.

;;; Code:

(require 'use-package)

(use-package smartparens
  :custom
  ;; Disable overlay since we use show-paren
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  :config
  ;; require config for most languages
  (require 'smartparens-config)

  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
	     :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))))

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  (smartparens-global-mode 1))

(provide 'amber-smartparens)

;;; amber-smartparens.el ends here
