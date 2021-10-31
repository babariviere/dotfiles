;;; amber-format.el --- add formatting suppport -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Package-Requires: (format-all)


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

;; Add formatting support to programming languages.

;;; Code:

(use-package format-all
  :disabled
  :hook ((prog-mode . format-all-mode)
	 (format-all-mode . format-all-ensure-formatter))
  :general
  (amber/leader-keys
    "cf" '(format-all-buffer :wk "format buffer")))

(provide 'amber-format)

;;; amber-format.el ends here
