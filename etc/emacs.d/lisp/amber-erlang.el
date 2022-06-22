;;; amber-erlang.el --- Erlang support for Amber Emacs  -*- lexical-binding: t; -*-

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

(require 'erlang-start)
(when (executable-find "erl")
  (setq erlang-root-dir (file-truename (concat (executable-find "erl") "../../lib/erlang"))
        erlang-man-root-dir (concat erlang-root-dir "/man")))

(provide 'amber-erlang)
;;; amber-erlang.el ends here
