(use-package general
  :config
  (general-create-definer leader-define-key
    :prefix ",")

  (leader-define-key
    :states 'normal
    "bk" 'kill-buffer))

(use-package evil
  :after general
  :config
  (evil-mode 1)
  ;; use evil mode in the buffer created from calling `list-packages'.
  (add-to-list 'evil-buffer-regexps '("*Packages*" . normal))

  ;; Force list-packages to use vim keybindings
  (with-eval-after-load 'package
    ;; movement keys j,k,l,h set up for free by defaulting to normal mode.
    ;; mark, unmark, install
    (general-define-key
     :states 'normal
     :keymaps 'package-menu-mode-map
     "m" 'package-menu-mark-install
     "u" 'package-menu-mark-unmark
     "x" 'package-menu-execute))
  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  (general-define-key
   :states 'insert
   "C-k" nil))
