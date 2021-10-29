;;; amber-nix.el --- Nix support -*- lexical-binding: t -*-

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

;; Add nix support to Amber Emacs

;;; Code:


(require 'use-package)

(defun amber/nix-set-formatter ()
  "Set default nix formatter for format-all."
  (setq-local format-all-formatters '(("Nix" nixfmt))))

(defun amber/nix-init ()
  "Init hook to initialize nix-mode."
  (add-to-list 'company-backends 'company-nixos-options nil t))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook ((format-all-mode . amber/nix-set-formatter)
		 (nix-mode . amber/nix-init))
  :init
  (add-to-list 'auto-mode-alist
               (cons "/flake\\.lock\\'"
                     'js-mode))
  ;; (amber/set-company-backend 'nix-mode 'company-nixos-options)
  :general
  (amber/leader-keys nix-mode-map
    "C-f" '(nix-format-buffer :wk "format buffer")
    "C-r" '(nix-repl-show :wk "repl")
    "C-s" '(nix-shell :wk "nix-shell")))

(use-package nix-drv-mode
  :mode "\\.drv\\'")

(use-package nix-update
  :commands nix-update-fetch)

(use-package nix-repl
  :commands nix-repl-show)


(provide 'amber-nix)

;;; amber-nix.el ends here
