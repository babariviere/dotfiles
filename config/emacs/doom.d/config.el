;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(load! "theme.el")
(setq display-line-numbers-type 'relative)

;; :completion ivy
(setf (alist-get 't ivy-re-builders-alist) #'ivy--regex-plus)

;; :lang data
;; See commit https://github.com/hlissner/doom-emacs/commit/e24b8cd1d1e4ceb7e4454a267985b639d5e7eb06
(use-package! graphql-mode
  :mode "\\.gql\\'"
  :config (setq-hook! 'graphql-mode-hook tab-width graphql-indent-level))


;; :lang dart
(use-package! dart-mode
  :mode ("\\.dart$" . dart-mode))

;; :lang elixir
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
  (map!
   :leader
   :prefix "c"
   :desc "Execute Lens" :n "L" #'lsp-avy-lens))


(after! elixir-mode
  (sp-with-modes 'elixir-mode
    (sp-local-pair "\"\"\"" "\"\"\"")))

;; :lang go
(after! lsp-mode
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

;; :lang rust
;; (setq rustic-lsp-server 'rust-analyzer)
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer
        lsp-rust-analyzer-server-command '("~/.cargo/bin/ra_lsp_server")))

;; :lang org
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

(use-package! org-roam-server)

(after! org-journal
  (setq org-journal-dir org-directory
        org-journal-file-format "daily-%Y%m.org"
        org-journal-file-pattern (org-journal-dir-and-format->regex
                                  org-journal-dir org-journal-file-format)
        org-journal-file-type 'monthly)

  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

  (setq org-journal-file-header 'org-journal-file-header-func)

  (add-to-list 'auto-mode-alist (cons org-journal-file-pattern 'org-journal-mode)))

;; :tools flyspell
(setq ispell-aspell-data-dir "/run/current-system/sw/lib/aspell"
      ispell-aspell-dict-dir ispell-aspell-data-dir
      ispell-aspell-dictionary-alist '())

;; :tools magit
(defadvice! +magit-invalidate-projectile-cache-a (&rest args)
  :after '(magit-checkout magit-branch-checkout)
  (projectile-invalidate-cache nil))

;; :ui deft
(after! deft
  (setq deft-directory "~/org"))

;; :ui pretty-code
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

;; :email
(set-email-account! "gmail"
                    '((mu4e-sent-folder       . "/gmail/[Gmail]/Sent Mail")
                      (mu4e-drafts-folder     . "/gmail/[Gmail]/Drafts")
                      (mu4e-trash-folder      . "/gmail/[Gmail]/Trash")
                      (mu4e-refile-folder     . "/gmail/[Gmail]/All Mail")
                      (smtpmail-smtp-user     . "babathriviere@gmail.com")
                      (user-mail-address      . "babathriviere@gmail.com")
                      (mu4e-compose-signature . "---\nBastien Rivière"))
                    t)

(after! mu4e
  (setq mu4e-index-cleanup t
        mu4e-index-lazy-check nil))

;; keybindings

(map!
 (:leader
  (:prefix "o"
   :desc "Email" :n "m" #'=mu4e)))

;; misc

(setq projectile-project-search-path '("/etc/dotfiles" "~/src" "~/src/github.com/babariviere" "~/projects"))

(use-package! systemd
  :defer)

(use-package! graphql
  :commands (graphql-query graphql-mutation))

(after! editorconfig
  (add-to-list 'auto-mode-alist '("\\editorconfig\\'" . editorconfig-conf-mode)))

;; safe variables
(add-to-list 'safe-local-variable-values '(go-tag-args . (lambda (x) (pcase x
                                                                       (`("-transform" ,_) t)
                                                                       (_ f)))))
(add-to-list 'safe-local-variable-values '(+format-on-save-enabled-modes . ()))

;; Load private configuration after everything else
(load! "private.el" doom-private-dir t)
