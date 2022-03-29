(require 'use-package)

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode 1)
  :general
  (:keymaps 'vertico-map
			"C-j" 'vertico-next
			"C-k" 'vertico-previous
			"C-f" 'vertico-exit))

(use-package savehist
  :init
  (savehist-mode 1))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1)
  :config
  (let ((categories '((persp-switch-to-buffer . buffer)
                      (projectile-find-file . project-file)
                      (projectile-recentf . project-file)
                      (projectile-switch-to-buffer . buffer)
                      (projectile-switch-project . project-file))))
    (dolist (category categories)
      (cl-pushnew category marginalia-command-categories :test #'equal))))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-component-separator "[ &]"))

(defun amber/consult-ripgrep-folder ()
  "Run consult-ripgrep in folder."
  (interactive)
  (consult-ripgrep default-directory))

(defun amber/consult-ripgrep-project ()
  "Run consult-ripgrep in projectile-project-root."
  (interactive)
  (consult-ripgrep (projectile-project-root)))

(use-package consult
  :general
  ([remap apropos]                       #'consult-apropos
   [remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   [remap goto-line]                     #'consult-goto-line
   [remap imenu]                         #'consult-imenu
   [remap locate]                        #'consult-locate
   [remap load-theme]                    #'consult-theme
   [remap man]                           #'consult-man
   [remap recentf-open-files]            #'consult-recent-file
   [remap switch-to-buffer]              #'consult-buffer
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
   [remap yank-pop]                      #'consult-yank-pop
   "C-s"  #'consult-line)
  (amber/leader-keys
    "s" '(:ignore t :wk "search")
    "sb" '(consult-line :wk "search buffer")
	"sd" '(amber/consult-ripgrep-folder :wk "search folder")
    "sp" '(amber/consult-ripgrep-project :wk "search project"))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function #'projectile-project-root
        consult-narrow-key "<"))

(use-package embark
  :general
  ("C-." '(embark-act :wk "act"))
  (amber/leader-keys
	"a" '(embark-act :wk "actions")
	"hb" '(embark-bindings :wk "describe bindings"))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'amber-completion)
