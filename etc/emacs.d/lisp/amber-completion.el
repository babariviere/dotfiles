;;; amber-completion.el --- Completion support for Amber  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Bastien Riviere

;; Author: Bastien Riviere(require 'use-package) <me@babariviere.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Completion support for Amber Emacs.

;;; Code:

(require 'use-package)
(require 'amber-project)

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode 1)
  :general
  (:keymaps 'vertico-map
			"C-j" 'vertico-next
			"C-k" 'vertico-previous
			"C-f" 'vertico-exit))

(use-package savehist
  :init
  (savehist-mode 1))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode 1)
  :config
  (let ((categories '((persp-switch-to-buffer . buffer))))
    (dolist (category categories)
      (cl-pushnew category marginalia-command-categories :test #'equal))))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-component-separator "[ &]"))

(defun amber-completion/consult-ripgrep-folder ()
  "Run consult-ripgrep in folder."
  (interactive)
  (consult-ripgrep default-directory))

(defun amber-completion/consult-ripgrep-project ()
  "Run consult-ripgrep in amber-project/root."
  (interactive)
  (consult-ripgrep (amber-project/root)))

(use-package consult
  :general
  ([remap apropos]                       #'consult-apropos
   [remap bookmark-jump]                 #'consult-bookmark
   [remap evil-show-marks]               #'consult-mark
   [remap goto-line]                     #'consult-goto-line
   [remap imenu]                         #'consult-imenu
   [remap locate]                        #'consult-locate
   [remap load-theme]                    #'consult-theme
   [remap man]                           #'consult-man
   [remap recentf-open-files]            #'consult-recent-file
   [remap switch-to-buffer]              #'consult-buffer
   [remap switch-to-buffer-other-window] #'consult-buffer-other-window
   [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
   [remap yank-pop]                      #'consult-yank-pop
   "C-s"  #'consult-line)
  (amber/leader-keys
    "s" '(:ignore t :wk "search")
    "sb" '(consult-line :wk "search buffer")
	"sd" '(amber-completion/consult-ripgrep-folder :wk "search folder")
    "sp" '(amber-completion/consult-ripgrep-project :wk "search project"))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-root-function #'amber-project/root
        consult-narrow-key "<"))

(use-package embark
  :general
  ("C-." '(embark-act :wk "act"))
  (amber/leader-keys
	"a" '(embark-act :wk "actions")
	"hb" '(embark-bindings :wk "describe bindings"))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'amber-completion)

;;; amber-completion.el ends here
