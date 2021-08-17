(require 'use-package)

(use-package counsel
  :bind (("M-x" . counsel-M-x)))

(use-package ivy
  :commands (swiper counsel-describe-function counsel-describe-variable)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package helpful
  :commands (helpful-key helpful-command)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(provide 'amber-completion)
