;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq
 doom-font (font-spec :family "Fira Mono" :size 12)
 doom-theme 'doom-dracula
 display-line-numbers-type t

 ;; lang rust
 rustic-lsp-server 'rust-analyzer)
