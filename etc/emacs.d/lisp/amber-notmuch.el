;;; amber-notmuch.el --- notmuch support for Amber Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <babariviere@gaia>
;; Keywords: mail

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

;; Add support for notmuch to Amber Emacs

;;; Code:

(require 'auth-source)

(with-eval-after-load 'notmuch
  (require 'notmuch)
  (setq notmuch-identities
        (let ((identities))
          (dolist (a (auth-source-search :port 993 :max 99))
            (push (format "%s <%s>" user-full-name (plist-get a :user)) identities))
          identities)
        notmuch-archive-tags '("-inbox" "+archive")
        notmuch-tagging-keys
        '(("a" notmuch-archive-tags "Archive")
          ("r" notmuch-show-mark-read-tags "Mark read")
          ("f" ("+flagged") "Flag (favorite)")
          ("s" ("+spam" "-inbox") "Mark as spam")
          ("d" ("+trash" "-inbox" "-archive") "Trash"))))


(let ((map global-map))
  (define-key map (kbd "C-c m") #'notmuch)
  (define-key map (kbd "C-x m") #'notmuch-mua-new-mail))

(provide 'amber-notmuch)
;;; amber-notmuch.el ends here
