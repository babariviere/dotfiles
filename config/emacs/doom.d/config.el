;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq
 doom-font (font-spec :family "Iosevka Baba" :size 13)
 doom-theme 'doom-dracula
 display-line-numbers-type t)

;;; :completion ivy
(setf (alist-get 't ivy-re-builders-alist) #'ivy--regex-plus)

;;; :lang rust
;; (setq rustic-lsp-server 'rust-analyzer)

;;; :tools flyspell
(setq ispell-aspell-data-dir "/run/current-system/sw/lib/aspell" )
(setq ispell-aspell-dict-dir ispell-aspell-data-dir)
(setq ispell-aspell-dictionary-alist '())

;;; :tools magit
(defadvice! +magit-invalidate-projectile-cache-a (&rest args)
  :after '(magit-checkout magit-branch-checkout)
  (projectile-invalidate-cache nil))

;;; :ui pretty-code
(load! "+iosevka.el")
(setq +pretty-code-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    ;; :true          "𝕋"
    ;; :false         "𝔽"
    ;; :int           "ℤ"
    ;; :float         "ℝ"
    ;; :str           "𝕊"
    ;; :bool          "𝔹"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•"))

;; misc

(use-package! systemd
  :defer)

;; safe variables
(add-to-list 'safe-local-variable-values '(go-tag-args . ("-transform" "camelcase")))
