;; Init configuration

;;; Code:
(set-frame-font "MonoLisa 12")
(load-theme 'kaolin-ocean t)

(add-to-list 'load-path "~/.emacs.d/lisp")
(load-library "amber-completion")
(load-library "amber-dired")
(load-library "amber-keys")
(load-library "amber-magit")
(load-library "amber-project")

(require 'use-package)

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-buffer-file-name-style 'relative-from-project))
