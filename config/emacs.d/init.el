;; Init configuration

(set-frame-font "MonoLisa 12")
(load-theme 'kaolin-ocean t)

(require 'use-package)

(use-package general
  :after evil
  :config
  (general-create-definer amber/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (amber/leader-keys
    "h"  '(:ignore h :which-key "help")
    "ht" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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

