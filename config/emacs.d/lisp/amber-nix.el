(require 'use-package)

(defun amber/nix-set-formatter ()
  "Set default nix formatter for format-all."
  (setq-local format-all-formatters '(("Nix" nixfmt))))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (format-all-mode . amber/nix-set-formatter)
  :init
  (add-to-list 'auto-mode-alist
               (cons "/flake\\.lock\\'"
                     'js-mode))
  (amber/set-company-backend 'nix-mode 'company-nixos-options)
  :general
  (amber/local-leader-keys nix-mode-map
    "f" '(nix-format-buffer :wk "format buffer")
    "r" '(nix-repl-show :wk "repl")
    "s" '(nix-shell :wk "nix-shell")))

(use-package nix-drv-mode
  :mode "\\.drv\\'")

(use-package nix-update
  :commands nix-update-fetch)

(use-package nix-repl
  :commands nix-repl-show)

(provide 'amber-nix)
