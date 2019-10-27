;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq
 doom-font (font-spec :family "Iosevka Baba" :size 13)
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
;; (setq rustic-lsp-server 'rust-analyzer)

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
