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

(provide 'amber-completion)
