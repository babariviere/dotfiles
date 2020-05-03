;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq
 doom-font (font-spec :family "@font@" :size 12)
 doom-theme '@doomTheme@
 display-line-numbers-type 'relative)

;;; :completion ivy
(setf (alist-get 't ivy-re-builders-alist) #'ivy--regex-plus)

;;; :lang dart
(use-package! dart-mode
  :mode ("\\.dart$" . dart-mode))

;; :lang elixir
;; (define-hostmode poly-elixir-hostmode
;;   :mode 'elixir-mode
;;   :protect-font-lock nil
;;   :protect-syntax t)
;;
;; (define-innermode poly-elixir-eex-metadata-innermode
;;   :mode 'web-mode
;;   :head-matcher "^.*\~L\"\"\"\n"
;;   :tail-matcher "^.*\"\"\"$"
;;   :head-mode 'host
;;   :tail-mode 'host)
;;
;; (define-polymode poly-elixir-mode
;;   :hostmode 'poly-elixir-hostmode
;;   :innermodes '(poly-elixir-eex-metadata-innermode))
;;
;; (add-hook 'poly-elixir-mode (lambda ()
;;                               (setq web-mode-engine "elixir"
;;                                     web-mode-content-type "html")))
;; (add-to-list 'auto-mode-alist '("\\.ex\\'" . poly-elixir-mode))
;;

(after! web-mode
  (setq web-mode-engines-alist
        '(("elixir"  . "\\.leex\\'")
          ("elixir"  . "\\.eex\\'")
          ("elixir"  . "\\.ex\\'"))
        ))

(defcustom lsp-elixir-suggest-specs t
  "Enable spec suggestion for ElixirLS."
  :type 'boolean
  :group 'lsp-elixir)

(after! lsp-mode
  (lsp-register-custom-settings
   '(("elixirLS.suggestSpecs" lsp-elixir-suggest-specs)))

  (add-hook 'lsp-after-initialize-hook
            (lambda ()
              (lsp--set-configuration (lsp-configuration-section "elixirLS"))))

  (require 'avy)
  (map! :leader
        :n
        :desc "Execute Lens" "c l" #'lsp-avy-lens))



;;; :lang rust
;; (setq rustic-lsp-server 'rust-analyzer)
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer
        lsp-rust-analyzer-server-command '("~/.cargo/bin/ra_lsp_server")))

;;; :lang org
(use-package! org-fancy-priorities ; priority icons
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("■" "■" "■")))

(after! org-roam
  (setq org-roam-directory org-directory
        org-roam-capture-templates  '(("d" "default" plain (function org-roam--capture-get-point)
                                       "%?"
                                       :file-name "${slug}"
                                       :head "#+TITLE: ${title}\n"
                                       :unnarrowed t
                                       :immediate-finish t))))

(after! org-journal
  (setq org-journal-dir org-directory
        org-journal-file-format "daily-%Y%m%d.org"
        org-journal-file-pattern (org-journal-dir-and-format->regex
                                  org-journal-dir org-journal-file-format))

  (add-to-list 'auto-mode-alist (cons org-journal-file-pattern 'org-journal-mode)))

;;; :tools flyspell
(setq ispell-aspell-data-dir "/run/current-system/sw/lib/aspell" )
(setq ispell-aspell-dict-dir ispell-aspell-data-dir)
(setq ispell-aspell-dictionary-alist '())

;;; :tools magit
(defadvice! +magit-invalidate-projectile-cache-a (&rest args)
  :after '(magit-checkout magit-branch-checkout)
  (projectile-invalidate-cache nil))

;;; :ui deft
(after! deft
  (setq deft-directory "~/org"))

;;; :ui pretty-code
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
        ;; :pipe          "" ;; FIXME: find a non-private char
        :dot           "•"))

;; :ui lsp

(add-hook 'lsp-mode-hook #'lsp-lens-mode)

;; misc

(use-package! systemd
  :defer)

(use-package! graphql
  :commands (graphql-query graphql-mutation))

;; safe variables
(add-to-list 'safe-local-variable-values '(go-tag-args . (lambda (x) (pcase x
                                                                       (`("-transform" ,_) t)
                                                                       (_ f)))))
(add-to-list 'safe-local-variable-values '(+format-on-save-enabled-modes . ()))
