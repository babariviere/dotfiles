;; Init configuration

;;; Code:
(let ((fixed-pitch '(:family "MonoLisa" :height 120)))
  (custom-set-faces
   `(default ((t ,fixed-pitch)))
   `(fixed-pitch ((t ,fixed-pitch)))))

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
(require 'amber-org)
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
