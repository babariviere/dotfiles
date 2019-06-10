(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-dynamic-exhibit-delay-ms 250)
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

(use-package projectile
  :after evil
  :config
  (projectile-mode +1)
  (leader-define-key
    :states 'normal
    "p" 'projectile-command-map)
  (setq projectile-generic-command "fd . -0 -c never --ignore-file .gitignore 2>/dev/null")
  (setq projectile-completion-system 'ivy)

  ;; add project root files
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))

(use-package counsel-projectile
  :config
  (add-hook 'after-init-hook 'counsel-projectile-mode))

(defadvice projectile-on (around exlude-tramp activate)
  "This should disable projectile when visiting a remote file."
  (unless  (--any? (and it (file-remote-p it))
		   (list
		    (buffer-file-name)
		    list-buffers-directory
		    default-directory
		    dired-directory))
    ad-do-it))

(use-package ace-window
	:after evil
	:config
	(leader-define-key
		:states 'normal
		"w" 'ace-window))
