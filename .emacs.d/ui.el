;; remove unecessary ui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; font
(set-face-attribute 'default nil :family "Iosevka Term")

;; line numbers
(global-display-line-numbers-mode t)

;; disable line number in modeline
(line-number-mode 0)
(column-number-mode 0)

;; use symbols
(global-prettify-symbols-mode t)

;; highlight current line
(global-hl-line-mode)

;; replace yes or no by y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set theme
(use-package doom-themes
  :config
  (load-theme 'doom-Iosvkem t)
  (doom-themes-org-config))

;; all the icons
(use-package all-the-icons)

;; modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github t)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-env-version t))

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

(use-package nyan-mode
  :init
  (setq nyan-animate-nyancat t)
  (setq nyan-bar-length 50)
  (setq nyan-wavy-trail t)
  :config
  (nyan-mode))
