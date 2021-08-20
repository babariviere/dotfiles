;;; package -- Summary
;;; Commentary:
;;;
;;; Provides global keybindings.
;;;
;;; Code:

(require 'use-package)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-create-definer amber/leader-keys
    :states '(normal insert motion visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer amber/local-leader-keys
    :states '(normal insert motion visual emacs)
    :keymaps 'override
    :prefix ","
    :non-normal-prefix "M-,")

  ;; Required to avoid error with non-prefixed key. (official solution from general)
  (amber/local-leader-keys
   "" nil)

  (amber/leader-keys
   "." '(find-file :wk "find file")
   "," '(switch-to-buffer :wk "switch to buffer")
   ":" '(execute-extended-command :wk "M-x")
   ";"  '(eval-expression :which-key "eval expression")
   "b"  '(:ignore t :wk "buffer")
   "bk" '(kill-current-buffer :wk "kill buffer")
   "h"  '(:ignore t :which-key "help")
   "hK" '(general-describe-keybindings :wk "describe keybindings")
   "hF" '(describe-face :wk "describe face")
   "ht" '(load-theme :wk "choose theme")))

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
  :general
  ([remap describe-function] 'helpful-function
   [remap describe-command] 'helpful-command
   [remap describe-variable] 'helpful-variable
   [remap describe-key] 'helpful-key
   [remap describe-symbol] 'helpful-symbol)
  (amber/leader-keys
   "hf" '(helpful-callable :wk "describe function")
   "hk" '(helpful-key :wk "describe key")
   "hv" '(helpful-variable :wk "describe variable")))

(provide 'amber-keys)
;;; amber-keys.el ends here
