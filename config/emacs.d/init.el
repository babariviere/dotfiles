;; Init configuration

;;; Code:
(setf (alist-get 'font default-frame-alist)
      "MonoLisa-12")
(custom-set-faces `(fixed-pitch
                    ((t (:font ,(font-spec :family "MonoLisa" :size 12)
                         :weight unspecified :slant unspecified :width unspecified)))))
(load-theme 'kaolin-ocean t)

(add-to-list 'load-path "~/.emacs.d/lisp")
(load-library "amber-keys")             ; must be loaded first

(load-library "amber-completion")
(load-library "amber-dired")
(load-library "amber-lisp")
(load-library "amber-magit")
(load-library "amber-project")
(load-library "amber-company")

(load-library "amber-elisp")
(load-library "amber-nix")

(require 'use-package)

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-buffer-file-name-style 'relative-from-project))
