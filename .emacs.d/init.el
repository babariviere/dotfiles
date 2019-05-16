;;; babariviere's configuration

(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

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

;; ensure all packages are installed
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; auto compile packages
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; Auto update packages
(use-package auto-package-update)

;; Pinentry in emacs
(use-package pinentry
  :config
  (pinentry-start))

(defvar config-files
  '("private.el"
    "ui.el" "keys.el"
    "check.el" "completion.el"
    "snippets.el" "direnv.el"
    "navigation.el"
    "org.el" "git.el"

    ;; Languages
    "lsp.el"
    "lisp.el"
    "rust.el"
    "go.el"
    "markdown.el"
    "fish.el"
    "terraform.el"
    "dart.el"
    "yaml.el"
    "feature.el"
    )
  "A list of custom configuration file name.  Relative to 'user-emacs-directory'.")

(dolist (c config-files)
  (load (concat user-emacs-directory c)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" default)))
 '(ivy-virtual-abbreviate (quote full))
 '(package-selected-packages
   (quote
    (feature-mode go-add-tags gotest go-guru godoctor dart-mode company-terraform terraform-mode fish-mode go-mode cargo rust-mode toml-mode company-lsp lsp-ui lsp-mode forge ghub git-gutter evil-magit magit org-jira org-bullets evil-org counsel-projectile projectile ivy-rich ivy direnv yasnippet company-quickhelp company flycheck evil-surround evil general nyan-mode which-key rainbow-delimiters doom-modeline doom-themes auto-package-update auto-compile use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
