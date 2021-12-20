;;; amber-python.el --- Add python support -*- lexical-binding: t -*-

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

;; Add python support to Amber Emacs.

;;; Code:

(require 'use-package)

(use-package python
  :mode ("[./]flake8\\'" . conf-mode)
  :mode ("/Pipfile\\'" . conf-mode)
  :config
  (sp-local-pair 'python-mode "'" nil
                 :unless '(sp-point-before-word-p
                           sp-point-after-word-p
                           sp-point-before-same-p))
  :general
  (python-mode-map
   ;; interferes with smartparens
   "DEL" nil)
  :custom
  (python-indent-guess-indent-offset-verbose nil)
  (python-shell-interpreter "python3"))

(use-package poetry
  :after python
  :hook (python-mode . poetry-tracking-mode)
  :init
  (setq poetry-tracking-strategy 'switch-buffer))

(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server"))
  :hook (python-mode . (lambda ()
						 (require 'lsp-python-ms)
						 (lsp))))

(provide 'amber-python)

;;; amber-python.el ends here
