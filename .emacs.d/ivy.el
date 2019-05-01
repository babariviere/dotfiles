(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (leader-define-key
    :states 'normal
    "bs" 'ivy-switch-buffer)
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-k" 'ivy-previous-line
   "C-j" 'ivy-next-line))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1)
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-path-style 'abbrev))
