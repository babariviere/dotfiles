;;; amber-data.el --- support for data formats -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Package-Requires: (yaml-mode)


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

;; Add support for various data languages.

;;; Code:

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
		 ("\\.yaml\\'" . yaml-mode))
  :hook (yaml-mode . lsp))

(use-package gitlab-ci-mode
  :mode (("\\.gitlab-ci.yml\\'" . gitlab-ci-mode))
  :hook (gitlab-ci-mode . lsp)
  :config
  (push 'gitlab-ci-mode (lsp--client-major-modes (gethash 'yamlls lsp-clients))))

(provide 'amber-data)

;;; amber-data.el ends here
