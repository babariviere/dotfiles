;; Init configuration

;;; Code:
(setf (alist-get 'font default-frame-alist)
      "MonoLisa-12")
(custom-set-faces `(fixed-pitch
                    ((t (:font ,(font-spec :family "MonoLisa" :size 12)
                         :weight unspecified :slant unspecified :width unspecified)))))
(load-theme 'kaolin-ocean t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'amber-keys)             ; must be loaded first

(require 'amber-completion)
(require 'amber-dired)
(require 'amber-lisp)
(require 'amber-magit)
(require 'amber-project)
(require 'amber-company)
(require 'amber-lsp)
(require 'amber-snippets)
(require 'amber-smartparens)
(require 'amber-direnv)

(require 'amber-elisp)
(require 'amber-elixir)
(require 'amber-nix)

(require 'use-package)

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-buffer-file-name-style 'relative-from-project))

(use-package eldoc
  :hook (emacs-lisp-mode . eldoc-mode))
