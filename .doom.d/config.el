;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq
 doom-font (font-spec :family "Iosevka Term SS07" :size 13)
 doom-theme 'doom-dracula
 display-line-numbers-type t)

;;; :completion ivy
(setf (alist-get 't ivy-re-builders-alist) #'ivy--regex-plus)

;;; :lang org
(setq
 org-log-done 'time)

;;; :lang go
(add-hook! 'before-save-hook #'gofmt-before-save)

;;; :lang rust
(setq rustic-lsp-server 'rust-analyzer)
