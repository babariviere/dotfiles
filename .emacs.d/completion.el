(use-package company
  :bind (("C-SPC". company-complete))
  :config
  (global-company-mode)
  (setq company-require-match nil
	company-idle-delay .3
	company-echo-delay 0
	company-begin-commands '(self-insert-command)
	company-auto-complete-chars nil)
  ;; <return> is for windowed Emacs; RET is for terminal Emacs
  (dolist (key '("<return>" "RET"))
    ;; Here we are using an advanced feature of define-key that lets
    ;; us pass an "extended menu item" instead of an interactive
    ;; function. Doing this allows RET to regain its usual
    ;; functionality when the user has not explicitly interacted with
    ;; Company.
    (define-key company-active-map (kbd key)
      `(menu-item nil company-complete
		  :filter ,(lambda (cmd)
			     (when (company-explicit-action-p)
			       cmd)))))
  (general-define-key
   :keymaps 'company-active-map
   "<tab>" #'company-complete-selection
   "TAB" #'company-complete-selection
   "SPC" nil
   "C-j" #'company-select-next
   "C-k" #'company-select-previous))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode))

;; See https://www.emacswiki.org/emacs/CompanyMode#toc10
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand)))
	(if (check-expansion)
	    (company-complete-common)
	  (indent-for-tab-command)))))

(general-define-key
 :keymaps 'yas-minor-mode
 [tab] #'tab-indent-or-complete)
