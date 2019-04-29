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

;; (add-to-list 'default-frame-alist
;; 	     '(font . "Iosevka Term-10"))
;; (set-face-attribute 'default nil :font "Iosevka Term" :height 100)
;; (set-frame-font "Iosevka Term-10" nil t)

(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))
