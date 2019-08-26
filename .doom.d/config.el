;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq
 doom-font (font-spec :family "Iosevka Term" :size 13)
 doom-theme 'doom-dracula
 display-line-numbers-type t

 ;; lang rust
 rustic-lsp-server 'rust-analyzer)

(add-hook! 'before-save-hook #'gofmt-before-save)
