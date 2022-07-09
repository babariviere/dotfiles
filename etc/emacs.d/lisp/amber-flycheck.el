;;; amber-flycheck.el --- Flycheck support -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Version: version
;; Package-Requires: (flycheck)
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

;; Add support for flycheck to Amber Emacs.

;;; Code:

(require 'use-package)

(use-package flycheck-inline
  :custom-face
  (flycheck-inline-info ((t (:inherit flycheck-error-list-info))))
  (flycheck-inline-error ((t (:inherit flycheck-error-list-error))))
  (flycheck-inline-warning ((t (:inherit flycheck-error-list-warning))))
  :hook (flycheck-mode . flycheck-inline-mode))

(provide 'amber-flycheck)

;;; amber-flycheck.el ends here
