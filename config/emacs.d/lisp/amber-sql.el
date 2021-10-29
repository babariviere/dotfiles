;;; amber-sql.el --- SQL support -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Version: version
;; Package-Requires: (sql)
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

;; Add SQL support for amber emacs.

;;; Code:

(require 'use-package)

(use-package sql
  :general
  (amber/leader-keys sql-mode-map
    "C-e" '(:ignore t :wk "eval")
    "C-e C-e" '(sql-send-paragraph :wk "eval")
    "C-e C-b" '(sql-send-buffer :wk "eval buffer")
    "C-e C-r" '(sql-send-region :wk "eval region")))

(provide 'amber-sql)

;;; amber-sql.el ends here
