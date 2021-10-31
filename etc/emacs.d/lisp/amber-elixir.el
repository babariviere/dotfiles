;;; amber-elixir.el --- Elixir support -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Version: version
;; Package-Requires: (elixir-mode alchemist)
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

;; Add support for elixir to amber emacs.

;;; Code:

(require 'use-package)

(use-package elixir-mode
  :hook ((elixir-mode . lsp)))

(use-package alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (with-eval-after-load 'evil-collection
    (general-def 'normal alchemist-mode-map
      "K" nil
      :package 'alchemist))
  :general
  (amber/leader-keys elixir-mode-map
    "C-m" '(alchemist-mix :wk "mix")
    "C-c" '(alchemist-mix-compile :wk "compile")
    "C-i" '(alchemist-iex-project-run :wk "iex run")
    "C-f" '(elixir-format :wk "format")
    "C-e" '(:ignore t :wk "eval")
    "C-e C-e" '(alchemist-iex-send-last-sexp :wk "eval sexp")
    "C-e C-r" '(alchemist-iex-send-region :wk "eval region")
    "C-e C-l" '(alchemist-iex-send-current-line :wk "eval line")
    "C-e C-R" '(alchemist-iex-reload-module :wk "reload module")))

(use-package exunit
  :hook (elixir-mode . exunit-mode)
  :general
  (amber/leader-keys elixir-mode-map
    "C-t" '(:ignore t :wk "test")
    "C-t C-a" '(exunit-verify-all :wk "verify all")
    "C-t C-r" '(exunit-rerun :wk "rerun")
    "C-t C-s" '(exunit-verify-single :wk "verify single")
    "C-t C-T" '(exunit-toggle-file-and-test :wk "toggle test file")
    "C-t C-t" '(exunit-toggle-file-and-test-other-window :wk "toggle test file (other)")
    "C-t C-v" '(exunit-verify :wk "verify")))

(provide 'amber-elixir)

;;; amber-elixir.el ends here
