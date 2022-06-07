(require 'use-package)

(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(use-package corfu
  :custom
  (corfu-cycle nil)
  (corfu-auto t)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match nil)
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)
  (corfu-echo-documentation nil)        ; we have corfu-doc
  :init
  (global-corfu-mode)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-quit-at-boundary t
                          corfu-quit-no-match t
                          corfu-auto nil
                          tab-always-indent 'complete)
              (corfu-mode)))


  ;; Silence the pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package cape
  :init
  ;; TODO: add them per buffer like tempel
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :hook (corfu-mode . corfu-doc-mode)
  :config
  (setq corfu-doc-delay 0.3))

(require 'pcmpl-args)

(provide 'amber-corfu)
