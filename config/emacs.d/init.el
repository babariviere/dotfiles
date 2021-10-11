;; Init configuration

;;; Code:
(let ((fixed-pitch '(:family "MonoLisa" :height 90)))
  (custom-set-faces
   `(default ((t ,fixed-pitch)))
   `(fixed-pitch ((t ,fixed-pitch)))))

(load-theme 'kaolin-ocean t)

(deftheme amber)
(enable-theme 'amber)
(setq custom-enabled-themes
	  (remq 'amber custom-enabled-themes))
(custom-theme-set-variables
 'amber
 '(kaolin-git-gutter-solid t)
 '(kaolin-themes-git-gutter-solid t)
 ;; Fix warning about not being able to determine a suitable EmacsClient
 '(with-editor-emacsclient-executable "emacsclient")

 '(indent-tabs-mode nil)
 '(tab-width 4))

(when (or (memq window-system '(mac ns x))
		  (daemonp))
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'amber-keys)             ; must be loaded first

(require 'amber-completion)
(require 'amber-dired)
(require 'amber-lisp)
(require 'amber-magit)
(require 'amber-project)
(require 'amber-company)
(require 'amber-lsp)
(require 'amber-snippets)
(require 'amber-smartparens)
(require 'amber-direnv)
(require 'amber-vterm)
(require 'amber-format)
(require 'amber-flycheck)
(require 'amber-persp)

(require 'amber-cc)
(require 'amber-common-lisp)
(require 'amber-csharp)
(require 'amber-data)
(require 'amber-elisp)
(require 'amber-elixir)
(require 'amber-fish)
(require 'amber-go)
(require 'amber-org)
(require 'amber-nix)
(require 'amber-python)
(require 'amber-sql)

(require 'use-package)

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode))

;; TODO: customize keyword faces
(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :config
  (global-hl-todo-mode 1))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
		 (prog-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; use margin instead of fringe
  (define-fringe-bitmap 'amber/diff-hl-fringe [224]
    nil nil '(center repeated))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'amber/diff-hl-fringe)
		diff-hl-margin-symbols-alist '((insert . " ")
									   (delete . " ")
									   (change . " ")
									   (unknown . " ")
									   (ignored . " ")))
  (diff-hl-margin-mode))

;; (use-package zoom
;;   :demand t
;;   :custom
;;   (zoom-size '(0.618 . 0.618))
;;   :config
;;   (zoom-mode 1))

(use-package pinentry
  :demand t
  :custom
  (epg-pinentry-mode 'loopback)
  :config
  (pinentry-start))

;; HACK: Fix issue with browse-url and wayland
(defun amber/enforce-display-env (&rest args)
  "Enforce DISPLAY env to be correctly set.

ARGS are the arguments passed to `browse-url`."
  (setenv "DISPLAY" ":0"))

(advice-add 'browse-url :before #'amber/enforce-display-env)
