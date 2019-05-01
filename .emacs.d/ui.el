;; remove unecessary ui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; line numbers
(global-display-line-numbers-mode 1)

;; use symbols
(global-prettify-symbols-mode t)

;; highlight current line
(global-hl-line-mode)

;; Set theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;; all the icons
(use-package all-the-icons)

;; modeline
(use-package doom-modeline
      :hook (after-init . doom-modeline-mode))

;; delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(show-paren-mode 1)

(use-package which-key
  :config
  (which-key-mode))

;; scroll compilation output
(setq compilation-scroll-output t)
