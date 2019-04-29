;; remove unecessary ui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; line numbers
(global-linum-mode 1)

;; Set theme
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(show-paren-mode 1)
