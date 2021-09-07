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

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(provide 'amber-go)

;;; amber-go.el ends here
