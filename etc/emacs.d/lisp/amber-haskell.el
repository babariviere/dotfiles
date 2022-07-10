;;; amber-haskell.el --- Haskell support for Amber Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Bastien Riviere

;; Author: Bastien Riviere <me@babariviere.com>
;; Keywords: languages

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

(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)
(add-hook 'haskell-mode-hook #'haskell-auto-insert-module-template)
(add-hook 'haskell-mode-hook #'haskell-decl-scan-mode)

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(setq haskell-tags-on-save nil
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t)

(defun amber-haskell/project-try-stack (dir)
  "Find super-directory of DIR containing stack.yaml file."
  (when-let (root (locate-dominating-file dir "stack.yaml"))
    (cons 'stack root)))

(cl-defmethod project-root ((project (head stack)))
  "Return PROJECT root."
  (cdr project))

(add-hook 'project-find-functions #'amber-haskell/project-try-stack)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(haskell-mode . ("haskell-language-server" "--lsp"))))

(provide 'amber-haskell)
;;; amber-haskell.el ends here
