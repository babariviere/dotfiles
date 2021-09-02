;; Init configuration

;;; Code:
(let ((fixed-pitch '(:family "MonoLisa" :height 120)))
  (custom-set-faces
   `(default ((t ,fixed-pitch)))
   `(fixed-pitch ((t ,fixed-pitch)))))

(load-theme 'kaolin-ocean t)
(custom-set-variables
 '(kaolin-git-gutter-solid t)
 '(kaolin-themes-git-gutter-solid t)
 ;; Fix warning about not being able to determine a suitable EmacsClient
 '(with-editor-emacsclient-executable "emacsclient"))

(when (memq window-system '(mac ns x))
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

(require 'amber-data)
(require 'amber-elisp)
(require 'amber-elixir)
(require 'amber-org)
(require 'amber-nix)
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

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package git-gutter-fringe
  :config
  ;; standardize default fringe width
  (if (fboundp 'fringe-mode) (fringe-mode '4))

  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom))

;; temporary, I want to find a solution for macOS
(server-start)
