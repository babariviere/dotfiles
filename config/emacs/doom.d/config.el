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

;;; :lang rust
;; (setq rustic-lsp-server 'rust-analyzer)
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer
        lsp-rust-analyzer-server-command '("~/.cargo/bin/ra_lsp_server")))

;;; :lang org
;; TODO: wait for +roam flags ?
;; https://github.com/hlissner/doom-emacs/pull/2764
;;
;; this config breaks emacs
;; (use-package! org-roam
;;   :hook ((org-mode . org-roam-mode)
;;          (after-init . org-roam--build-cache-async))
;;   :commands (org-roam org-roam-today org-roam-show-graph
;;                       org-roam-find-file org-roam-insert)
;;   :custom
;;   (org-roam-directory "~/org"))

(map!
 :leader
 (:prefix ("n r" . "roam")
   "r" #'org-roam
   "t" #'org-roam-today
   "f" #'org-roam-find-file
   "i" #'org-roam-insert
   "g" #'org-roam-show-graph))


(use-package! org-fancy-priorities ; priority icons
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("■" "■" "■")))

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
(load! "modules/+iosevka.el")
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

(use-package! graphql
  :commands (graphql-query graphql-mutation))

;; safe variables
(add-to-list 'safe-local-variable-values '(go-tag-args . (lambda (x) (pcase x
                                                                       (`("-transform" ,_) t)
                                                                       (_ f)))))
(add-to-list 'safe-local-variable-values '(+format-on-save-enabled-modes . ()))

;; temporary code to add support for ligatures
(use-package! composite
  :defer t
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  ;; support ligatures, some toned down to prevent hang
  (when (version<= "27.0" emacs-version)
    (let ((alist
           '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
             (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
             (36 . ".\\(?:\\(>\\)>?\\)")
             (37 . ".\\(?:\\(%\\)%?\\)")
             (38 . ".\\(?:\\(&\\)&?\\)")
             (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
             ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
             (43 . ".\\(?:\\([>]\\)>?\\)")
             ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
             (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
             ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
             (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
             (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
             ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
             (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
             (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
             (59 . ".\\(?:\\(;\\);?\\)")
             (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
             (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
             (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
             (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
             (91 . ".\\(?:\\(|\\)[]|]?\\)")
             ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
             (94 . ".\\(?:\\(=\\)=?\\)")
             (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
             (119 . ".\\(?:\\(ww\\)w?\\)")
             (123 . ".\\(?:\\(|\\)[|}]?\\)")
             (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
             (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
      (dolist (char-regexp alist)
        (set-char-table-range composition-ligature-table (car char-regexp)
                              `([,(cdr char-regexp) 0 font-shape-gstring]))))
    (set-char-table-parent composition-ligature-table composition-function-table)))
