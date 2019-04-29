;;; babariviere's configuration

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Safe theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("28bf1b0a72e3a1e08242d776c5befc44ba67a36ced0e55df27cfc7ae6be6c24d" default)))
 '(package-selected-packages
   (quote
    (org-evil org-mode ivy-rich projectile ivy direnv yasnippet all-the-icons unicode-fonts company company-go go-mode doom-modeline flycheck-rust cargo rust-mode toml-mode flycheck company-lsp lsp-ui lsp-mode use-package company-mode
	      (evil)
	      (evil)
	      night-owl-theme evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#011627" :foreground "#D6DEEB" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 96 :width normal :foundry "CYEL" :family "Iosevka Term")))))

;; Set backup directory
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
        (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . , --backup-directory)))

;; Set autosave directory
(defvar --autosave-directory (concat user-emacs-directory "autosaves/"))
(if (not (file-exists-p --autosave-directory))
    (make-directory --autosave-directory t))
(setq auto-save-file-name-transforms
      `((".*" , --autosave-directory t)))


;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defvar config-files
  '("ui.el" "evil.el"
    "flycheck.el" "company.el"
    "yas.el" "direnv.el"
    "ivy.el" "projectile.el"
    "org.el"

    ;; Languages
    "lsp.el"
    "lisp.el"
    "rust.el"
    "go.el"
    )
  "A list of custom configuration file name.  Relative to 'user-emacs-directory'.")

(dolist (c config-files)
  (load (concat user-emacs-directory c)))
