(defvar leader-key ",")

(defun leader (keys)
  "add leader key prefix"
  (concat leader-key " " keys))

(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  ;; use evil mode in the buffer created from calling `list-packages'.
  (add-to-list 'evil-buffer-regexps '("*Packages*" . normal))

  ;; Force list-packages to use vim keybindings
  (with-eval-after-load 'package
    ;; movement keys j,k,l,h set up for free by defaulting to normal mode.
    ;; mark, unmark, install
    (evil-define-key 'normal package-menu-mode-map (kbd "m") #'package-menu-mark-install)
    (evil-define-key 'normal package-menu-mode-map (kbd "u") #'package-menu-mark-unmark)
    (evil-define-key 'normal package-menu-mode-map (kbd "x") #'package-menu-execute))

  (defvar leader-map (make-sparse-keymap)
    "Keymap for leader key shortcuts.")
  (define-key evil-normal-state-map leader-key leader-map))
