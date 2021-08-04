;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bastien Riviere"
      user-mail-address "babathriviere@gmail.com")

(setq doom-private-dir "~/src/github.com/babariviere/dotfiles/config/doom.d")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "MonoLisa" :size 12))
;;doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'kaolin-ocean)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/src/github.com/babariviere/notes")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,")

(after! projectile
  (setq projectile-project-search-path '("~/src" "~/src/github.com/babariviere"))
  (setq projectile-project-root-files-functions '(projectile-root-local
                                                  projectile-root-top-down
                                                  projectile-root-bottom-up
                                                  projectile-root-top-down-recurring)))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! lsp-mode
  (setq lsp-elixir-server-command '("elixir-ls"))) ; REVIEW(babariviere): inject elixir from nix store

(use-package! parrot
  :commands (parrot-mode)
  :config
  (setq parrot-num-rotations nil))

(use-package! nyan-mode
  :commands (nyan-mode)
  :config
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t))

(after! ivy-posframe
  :config
  (setq ivy-posframe-display-functions-alist
        '((t . ivy-posframe-display-at-frame-top-center))))

;; Sly
(after! sly
  (require 'eros)
  (defun +common-lisp--sly-eval-last-sexp ()
    "Eval last lisp expression and print it with eros"
    (interactive)
    (cl-destructuring-bind (output value) (sly-eval `(slynk:eval-and-grab-output ,(sly-last-expression)))
      (eros--make-result-overlay (concat output value)
        :where (point)
        :duration eros-eval-result-duration)))

  (unless evil-move-beyond-eol
    (advice-add '+common-lisp--sly-eval-last-sexp :around 'evil-collection-sly-last-sexp))

  (map! (:localleader
         :map lisp-mode-map
         (:prefix "e"
          (:desc "Evaluate last" "e" #'+common-lisp--sly-eval-last-sexp)))))

(after! yasnippet
  (add-hook 'snippet-mode-hook (lambda () (setq require-final-newline nil))))

;; Dhall
(use-package! dhall-mode
  :mode "\\.dhall\\'")

(after! lsp-mode
  (add-hook 'dhall-mode #'lsp-mode))

(use-package! kaolin-themes
  :config
  (setq kaolin-themes-modeline-border nil
        kaolin-themes-comments-style 'normal))
