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
    ;; :states '(normal insert motion visual emacs)
    :keymaps 'override
    :prefix "C-c")                    ; switch everything to C-c

  (amber/leader-keys
    "." '(find-file :wk "find file")
    "b"  '(:ignore t :wk "buffer")
    "bk" '(kill-current-buffer :wk "kill buffer")
    "h"  '(:ignore t :which-key "help")
    "ht" '(load-theme :wk "choose theme")))

(defun amber/window-split-and-follow ()
  "Split current window horizontally, then focus new window.
If `evil-split-window-below' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-split-window-below (not evil-split-window-below)))
    (call-interactively #'evil-window-split)))

(defun amber/window-vsplit-and-follow ()
  "Split current window vertically, then focus new window.
If `evil-vsplit-window-right' is non-nil, the new window isn't focused."
  (interactive)
  (let ((evil-vsplit-window-right (not evil-vsplit-window-right)))
    (call-interactively #'evil-window-vsplit)))

(use-package evil
  :disabled
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-i-jump nil)
  (evil-undo-system 'undo-fu)
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
    "wl" '(evil-window-right :wk "right window")
    "wH" '(evil-window-move-far-left :wk "move left")
    "wJ" '(evil-window-move-very-bottom :wk "move bottom")
    "wK" '(evil-window-move-very-top :wk "move top")
    "wL" '(evil-window-move-far-right :wk "move right")
    "ws" '(evil-window-split :wk "split horizontal")
    "wS" '(amber/window-split-and-follow :wk "split horizontal (follow)")
    "wv" '(evil-window-vsplit :wk "split vertical")
    "wV" '(amber/window-vsplit-and-follow :wk "split vertical (follow)")))

(use-package evil-collection
  :disabled
  :after evil
  :custom
  (evil-collection-key-blacklist '("SPC" "," "gd" "K"))
  :config
  (mapc (lambda (x) (delq x evil-collection-mode-list))
        '(lispy))
  (evil-collection-init))

(use-package evil-surround
  :disabled
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :disabled
  :after evil
  :config
  (evil-commentary-mode 1))

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

(use-package undo-fu
  :custom
  (undo-limit 400000)
  (undo-strong-limit 3000000)
  (undo-outer-limit 48000000)
  :config
  (define-minor-mode global-undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo)
              (define-key map (kbd "C-/")     #'undo-fu-only-undo)
              (define-key map (kbd "M-/")     #'undo-fu-only-redo)
              (define-key map (kbd "C-M-_")   #'undo-fu-only-redo-all)
              (define-key map (kbd "C-x r u") #'undo-fu-session-save)
              (define-key map (kbd "C-x r U") #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t)
  (global-undo-fu-mode 1))

;; TODO: undo-fu-session-directory in cache
(use-package undo-fu-session
  :hook (global-undo-fu-mode . global-undo-fu-session-mode)
  :custom
  (undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(provide 'amber-keys)
;;; amber-keys.el ends here