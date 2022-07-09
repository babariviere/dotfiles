(require 'use-package)

(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-echo-documentation 0.25)
  :init
  (global-corfu-mode)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-quit-at-boundary t
                          corfu-quit-no-match t
                          corfu-auto nil
                          tab-always-indent 'complete)
              (corfu-mode))))


(require 'cape)

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-tex)
(add-to-list 'completion-at-point-functions #'cape-ispell)
(add-to-list 'completion-at-point-functions #'cape-symbol)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Silence the pcomplete capf, no errors or messages!
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

;; Ensure that pcomplete does not write to the buffer
;; and behaves as a pure `completion-at-point-function'.
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(require 'pcmpl-args)

(provide 'amber-corfu)
