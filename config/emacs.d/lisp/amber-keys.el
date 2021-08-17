;;; package -- Summary
;;; Commentary:
;;;
;;; Provides global keybindings.
;;;
;;; Code:

(require 'use-package)

(use-package general
  :after evil
  :config
  (general-create-definer amber/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer amber/local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "M-SPC")

  (amber/leader-keys
   "."  '(counsel-switch-buffer :which-key "switch buffer")
   ";"  '(eval-expression :which-key "eval expression")
   ":"  '(counsel-M-x :which-key "M-x")
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

(provide 'amber-keys)
;;; amber-keys.el ends here
