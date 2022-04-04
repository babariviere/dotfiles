;;; amber-elfeed.el --- elfeed support for Amber Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <babariviere@geras>
;; Keywords: news

;; This program is free software; you can redistribute it and/or modify
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

;;

;;; Code:

(require 'use-package)

(use-package elfeed
  :bind
  ("C-x w" . elfeed))

(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files (list (concat org-directory "elfeed.org")))
  (elfeed-org))

(provide 'amber-elfeed)
;;; amber-elfeed.el ends here
