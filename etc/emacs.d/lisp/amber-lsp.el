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

(require 'eglot)

(define-key eglot-mode-map (kbd "C-c l R") 'eglot-reconnect)
(define-key eglot-mode-map (kbd "C-c l S") 'eglot-shutdown)
(define-key eglot-mode-map (kbd "C-c l a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c l f") 'eglot-format)
(define-key eglot-mode-map (kbd "C-c l h") 'eglot)
(define-key eglot-mode-map (kbd "C-c l o") 'eglot-code-action-organize)
(define-key eglot-mode-map (kbd "C-c l r") 'eglot-rename)

(provide 'amber-lsp)

;;; amber-lsp.el ends here
