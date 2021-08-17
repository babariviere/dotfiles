;; Init configuration

;;; Code:
(set-frame-font "MonoLisa 12")
(load-theme 'kaolin-ocean t)

(add-to-list 'load-path "~/.emacs.d/lisp")
(load-library "amber-keys")
(load-library "amber-magit")

(require 'use-package)

(use-package counsel
  :bind (("M-x" . counsel-M-x)))

(use-package ivy
  :commands (swiper counsel-describe-function counsel-describe-variable)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package helpful
  :commands (helpful-key helpful-command)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :demand
  :config (which-key-mode 1)
  :custom
  (which-key-idle-delay 1))

(use-package projectile
  :after general
  :config (projectile-mode 1)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action #'projectile-dired)
  :general
  (amber/leader-keys
   "p" '(:keymap projectile-command-map :which-key "project")))

(use-package counsel-projectile
  :config (counsel-projectile-mode 1))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diff-hl
  :hook ((dired-mode . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))

