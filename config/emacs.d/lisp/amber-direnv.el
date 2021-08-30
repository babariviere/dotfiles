;;; amber-direnv.el --- Direnv support -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Package-Requires: (envrc)

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

;; Add support for direnv .envrc files loading.

;;; Code:

(defun amber/direnv-init-earlier-h ()
  "Initialize direnv before major hook init."
  (let ((fn #'envrc-global-mode-enable-in-buffers))
    (if (not envrc-global-mode)
	(remove-hook 'change-major-mode-after-body-hook fn)
      (remove-hook 'after-change-major-mode-hook fn)
      (add-hook 'change-major-mode-after-body-hook fn 100))))

(use-package envrc
  :config
  (envrc-global-mode 1)
  (amber/direnv-init-earlier-h))

(provide 'amber-direnv)

;;; amber-direnv.el ends here
