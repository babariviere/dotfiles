;;; amber-lsp.el --- summary -*- lexical-binding: t -*-

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

;; LSP support to Amber Emacs.

;;; Code:

(require 'use-package)

;; TODO: look at the bug fixes in this file
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/tools/lsp/%2Blsp.el

(use-package lsp-mode
  :demand t
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-enable-folding nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-diagnostics t)
  ;; breaks sideline
  (lsp-eldoc-enable-hover nil)
  (lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package consult-lsp
  :general
  (lsp-mode-map
   [remap xref-find-apropos] #'consult-lsp-symbols))

(provide 'amber-lsp)

;;; amber-lsp.el ends here
