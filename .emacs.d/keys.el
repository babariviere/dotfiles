(use-package general
  :config
  (general-create-definer leader-define-key
    :prefix ",")

  (leader-define-key
    :states 'normal
    "bk" 'kill-buffer))

(use-package evil
  :after general
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  (general-define-key
   :states 'insert
   "C-k" nil))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
