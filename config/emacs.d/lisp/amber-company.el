(require 'use-package)

;; Stolen from doom emacs. Maybe rework it for my own need.
;; TODO: add general keyword to hook company backend
(defvar amber/company-backend-alist
  '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
    (prog-mode company-capf company-yasnippet)
    (conf-mode company-capf company-dabbrev-code company-yasnippet)))

(defun amber/company--backends ()
  "Get list of all backends for the `major-mode'."
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode amber/company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in amber/company-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))

;; (let ((major-mode 'emacs-lisp-mode))
;;   (amber/company--backends))

(defun amber/company-init-backends ()
  "Initialize backends for the buffer."
  (or (memq major-mode '(fundamental-mode special-mode))
      buffer-read-only
      (setq-local company-backends (amber/company--backends))))

(defun amber/set-company-backend (modes &rest backends)
  "Set company BACKENDS for MODES."
  (dolist (mode (if (listp modes) modes (list modes)))
    (if (null (car backends))
        (setq amber/company-backend-alist
              (delq (assq mode amber/company-backend-alist)
                    amber/company-backend-alist))
      (setf (alist-get mode amber/company-backend-alist)
            backends))))

(put 'amber/company-init-backends 'permanent-local-hook t)

(add-hook 'after-change-major-mode-hook #'amber/company-init-backends 'append)

(use-package company
  :hook
  (after-init . global-company-mode)
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0)
  (company-backends '(company-capf))
  (company-auto-commit nil)
  (company-tooltip-limit 14)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Only search the current buffer for `company-dabbrev' (a backend that
  ;; suggests text your open buffers). This prevents Company from causing
  ;; lag once you have a lot of buffers open.
  (company-dabbrev-other-buffers nil)
  ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
  ;; domain-specific words with particular casing.
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  :general
  (:keymaps 'company-active-map
   "C-j" #'company-select-next
   "C-k" #'company-select-previous
   "<tab>"  #'company-complete-common-or-cycle
   "RET" #'company-complete-selection)
  (:keymaps 'lsp-mode-map
   "C-j" #'company-select-next
   "C-k" #'company-select-previous
   "<tab>"  #'company-complete-common-or-cycle
   "RET" #'company-complete-selection))

(use-package company-box
  :hook (company-mode . company-box-mode))

(provide 'amber-company)
