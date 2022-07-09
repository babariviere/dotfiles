;;; package -- Summary
;;; Commentary:
;;;
;;; Provides global keybindings.
;;;
;;; Code:

(require 'use-package)
(require 'diminish)

(use-package general
  :config
  (general-create-definer amber/leader-keys
    ;; :states '(normal insert motion visual emacs)
    :keymaps 'override
    :prefix "C-c")                      ; switch everything to C-c

  (amber/leader-keys
    "." '(find-file :wk "find file")
    "b"  '(:ignore t :wk "buffer")
    "bk" '(kill-current-buffer :wk "kill buffer")
    "h"  '(:ignore t :which-key "help")
    "ht" '(load-theme :wk "choose theme")))

(use-package which-key
  :demand
  :config
  (which-key-mode 1)
  (diminish 'which-key-mode)
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
