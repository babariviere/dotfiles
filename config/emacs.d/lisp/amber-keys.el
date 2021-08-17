;;; package -- Summary
;;; Commentary:
;;;
;;; Provides global keybindings.
;;;
;;; Code:

(require 'use-package)

(use-package general
  :config
  (general-create-definer amber/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer amber/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC m"
    :non-normal-prefix "M-SPC")

  (amber/leader-keys
   "."  '(counsel-switch-buffer :which-key "switch buffer")
   ";"  '(eval-expression :which-key "eval expression")
   ":"  '(counsel-M-x :which-key "M-x")
   "h"  '(:ignore h :which-key "help")
   "hF" '(describe-face :wk "describe face")
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
  (evil-set-initial-state 'dashboard-mode 'normal)
  :general
  (amber/leader-keys
   "w" '(:ignore t :wk "window")
   "wh" '(evil-window-left :wk "left window")
   "wj" '(evil-window-down :wk "down window")
   "wk" '(evil-window-up :wk "up window")
   "wl" '(evil-window-right :wk "right window")))

(use-package evil-collection
  :after evil
  :config
  (mapc (lambda (x) (delq x evil-collection-mode-list))
        '(lispy))
  (evil-collection-init))

(use-package which-key
  :demand
  :config (which-key-mode 1)
  :custom
  (which-key-idle-delay 1))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  :general
  (amber/leader-keys
   "hf" '(helpful-callable :wk "describe function")
   "hk" '(helpful-key :wk "describe key")
   "hv" '(helpful-variable :wk "describe variable")))

(provide 'amber-keys)
;;; amber-keys.el ends here
