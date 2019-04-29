;; remove unecessary ui elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; line numbers
(global-linum-mode 1)

;; set theme
(use-package night-owl-theme
    :ensure t
    :init
    (load-theme 'night-owl))

(set-default-font "Iosevka Term")

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))
