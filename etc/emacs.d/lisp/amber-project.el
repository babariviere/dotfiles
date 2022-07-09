;;; amber-project.el --- Project support for Amber   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Bastien Riviere

;; Author: Bastien Riviere(require 'use-package) <me@babariviere.com>
;; Keywords: files, tools

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

;; Add support for projects in Amber Emacs.

;;; Code:

(require 'project)

(global-set-key (kbd "C-c SPC") #'project-find-file)

(defun amber-project/root (&optional path)
  "Return project root or PATH / `default-directory' if there is no current project."
  (car (last (project-current nil
                              (or path
                                  default-directory)))))

(provide 'amber-project)

;;; amber-project.el ends here
