;;; amber-vterm.el --- vterm support -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Package-Requires: (vterm)


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

;; Add vterm support to amber emacs.

;;; Code:

(require 'use-package)

(use-package vterm
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 5000)
  (vterm-buffer-name-string "vterm:%s")
  :general
  (amber/leader-keys
    "o" '(:ignore t :wk "open")
    "ot" '(vterm-other-window :wk "open vterm")
    "oT" '(vterm :wk "open vterm here")))


(provide 'amber-vterm)

;;; amber-vterm.el ends here
