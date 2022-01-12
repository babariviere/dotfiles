;; Init configuration

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(guix-emacs-autoload-packages)

(let ((fixed-pitch '(:family "MonoLisa" :height 90)))
  (custom-set-faces
   `(default ((t ,fixed-pitch)))
   `(fixed-pitch ((t ,fixed-pitch)))))

(require 'modus-themes)

;; These colors are inspired by:
;; https://github.com/rakr/vim-two-firewatch
;; https://simurai.com/projects/2016/01/01/duotone-themes
(custom-set-variables
 '(modus-themes-mixed-fonts t)
 '(modus-themes-italic-constructs t)
 '(modus-themes-region nil)
 '(modus-themes-links '(neutral-underline))
 '(modus-themes-mode-line '(accented moody borderless))
 '(modus-themes-operandi-color-overrides '((fg-main . "#000000")
					   (bg-main . "#faf8f5")
					   (bg-region . "#efdfff")
					   (bg-inactive . "#e6e4e1")
					   (bg-hl-line . "#e6e4e1")))
 '(modus-themes-vivendi-color-overrides '((fg-main . "#fdf3ec")
					  (bg-main . "#24242d")
					  (bg-region . "#4f3d88")
					  (bg-inactive . "#2f2f3b")
					  (bg-hl-line . "#2f2f3b"))))

(defun customize-modus-vivendi nil
  (custom-set-variables
   '(modus-themes-syntax '(yellow-comments alt-syntax faint green-strings))))

(defun customize-modus-operandi nil
  (custom-set-variables
   '(modus-themes-syntax '(yellow-comments))))

(advice-add 'modus-themes-load-vivendi :before 'customize-modus-vivendi)
(advice-add 'modus-themes-load-operandi :before 'customize-modus-operandi)

(modus-themes-load-operandi)

;; (require 'kaolin-themes)
;; (load-theme 'kaolin-valley-dark t)

(setq modus-themes-scale-headings t)
(setq mode-line-modes
      (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
		(list (propertize "%[" 'help-echo recursive-edit-help-echo)
			  "("
			  `(:propertize ("" mode-name)
							help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
							mouse-face mode-line-highlight
							local-map ,mode-line-major-mode-keymap)
			  '("" mode-line-process)
			  ")"
			  (propertize "%]" 'help-echo recursive-edit-help-echo)
			  " ")))
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

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
 '(tab-width 4)
 '(window-divider-default-right-width 8))

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
;; NOTE: Not sure I like it
;; (require 'amber-persp)

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
(require 'amber-scheme)
(require 'amber-sql)

(require 'use-package)

;; (use-package doom-modeline
;;   :config
;;   (doom-modeline-mode 1)
;;   :custom
;;   (doom-modeline-height 15)
;;   (doom-modeline-buffer-file-name-style 'relative-from-project))

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

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package crux
  :bind
  (([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ("<S-return>" . crux-smart-open-line)
   ("<C-S-return>" . crux-smart-open-line-above)))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-separator "/"))

(use-package ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-scope 'frame)
  :bind
  ("C-x o" . ace-window))

(use-package avy
  :bind
  ("C-;" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

(global-unset-key (kbd "C-z"))