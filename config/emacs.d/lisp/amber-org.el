;;; amber-org.el --- Org support -*- lexical-binding: t -*-

;; Author: Bastien Rivière
;; Maintainer: Bastien Rivière
;; Version: version
;; Package-Requires: (org evil-org org-contrib org-agenda org-superstar org-capture org-appear)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; Add org-mode support to amber emacs.

;;; Code:

(require 'use-package)

;; A slightly modified version of doom's one.
;; Source: https://github.com/hlissner/doom-emacs/blob/bf8495b4122701fb30cb6cea37281dc8f3bedcd0/modules/lang/org/autoload/org.el#L125
(defun amber/org-dwin-at-point (&optional arg)
  "Do-what-I-mean at point (org mode)."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
	   (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
	(setq context (org-element-property :parent context)
	      type (org-element-type context)))
      (pcase type
	(`headline
	 (cond ((memq (bound-and-true-p org-goto-map)
		      (current-active-maps))
		(org-goto-ret))
	       ((and (fboundp 'toc-org-insert-toc)
		     (member "TOC" (org-get-tags)))
		(toc-org-insert-toc)
		(message "Updating table of contents"))
	       ((string= "ARCHIVE" (car-safe (org-get-tags)))
		(org-force-cycle-archived))
	       ((or (org-element-property :todo-type context)
		    (org-element-property :scheduled context))
		(let ((org-use-fast-todo-selection nil))
		  (org-todo))))
	 ;; Update any metadata
	 (org-update-checkbox-count)
	 (org-update-parent-todo-statistics)
	 (when (and (fboundp 'toc-org-insert-toc)
		    (member "TOC" (org-get-tags)))
	   (toc-org-insert-toc)
	   (message "Updating table of contents")))

	(`clock (org-clock-update-time-maybe))

	(`footnote-reference
	 (org-footnote-goto-definition (org-element-property :label context)))

	(`footnote-definition
	 (org-footnote-goto-previous-reference (org-element-property :label context)))

	((or `planning `timestamp)
	 (org-follow-timestamp-link))

	((or `table `table-row)
	 (if (org-at-TBLFM-p)
	     (org-table-calc-current-TBLFM)
	   (ignore-errors
	     (save-excursion
	       (goto-char (org-element-property :contents-begin context))
	       (org-call-with-arg 'org-table-recalculate (or arg t))))))

	(`table-cell
	 (org-table-blank-field)
	 (org-table-recalculate arg)
	 (when (and (string-empty-p (string-trim (org-table-get-field)))
		    (bound-and-true-p evil-local-mode))
	   (evil-change-state 'insert)))

	(`babel-call
	 (org-babel-lob-execute-maybe))

	(`statistics-cookie
	 (save-excursion (org-update-statistics-cookies arg)))

	((or `src-block `inline-src-block)
	 (org-babel-execute-src-block arg))

	((or `latex-fragment `latex-environment)
	 (org-latex-preview arg))

	(`link
	 (let* ((lineage (org-element-lineage context '(link) t))
		(path (org-element-property :path lineage)))
	   (if (or (equal (org-element-property :type lineage) "img")
		   (and path (image-type-from-file-name path)))
	       (+org--toggle-inline-images-in-subtree
		(org-element-property :begin lineage)
		(org-element-property :end lineage))
	     (org-open-at-point arg))))

	((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
	 (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
	   (org-toggle-checkbox (if (equal match "[ ]") '(16)))))))))

(defun amber/org-slow-todo ()
  "Launch org-todo with fast-mode disabled."
  (interactive)
  (let ((org-use-fast-todo-selection 'auto))
    (org-todo)))

(defun amber/org-babel-lazy-load-h ()
  "Load babel libraries lazily when babel blocks are executed."
  (defun amber/org--babel-lazy-load (lang &optional async)
    (cl-check-type lang (or symbol null))
    (unless (cdr (assq lang org-babel-load-languages))
      (when async
        ;; ob-async has its own agenda for lazy loading packages (in the child
        ;; process), so we only need to make sure it's loaded.
        (require 'ob-async nil t))
      (prog1 (or (require (intern (format "ob-%s" lang)) nil t)
                 (require lang nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))))

  (defun amber/org--export-lazy-load-library-h ()
    "Lazy load a babel package when a block is executed during exporting."
    (amber/org--babel-lazy-load-library-a (org-babel-get-src-block-info)))

  (advice-add 'org-babel-exp-src-block :before #'amber/org--export-lazy-load-library-h)

  (defun amber/org--src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    (or (cdr (assoc lang org-src-lang-modes))
	(amber/org--babel-lazy-load lang)))

  (advice-add 'org-src--get-lang-mode :before #'amber/org--src-lazy-load-library-a)
  
  (defun amber/org--babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
	   ;; useful if we want to add aliases
           ;; (lang (or (cdr (assq lang +org-babel-mode-alist))
           ;;           lang))
	   )
      (amber/org--babel-lazy-load
       lang (and (not (assq :sync (nth 2 info)))
                 (assq :async (nth 2 info))))
      t))

  (advice-add 'org-babel-confirm-evaluate :after-while #'amber/org--babel-lazy-load-library-a)

  (advice-add #'org-babel-do-load-languages :override #'ignore))

(use-package org
  :hook ((org-mode . visual-line-mode)
	 (org-mode . amber/org-babel-lazy-load-h))
  :custom
  (org-hide-emphasis-marker t)
  (org-hide-leading-star nil)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-startup-indented t)
  (org-confirm-babel-evaluate nil)
  :general
  ('normal org-mode-map
  	   "RET" #'amber/org-dwin-at-point
	   [ret] #'amber/org-dwin-at-point)
  (amber/local-leader-keys org-mode-map
    "t" '(amber/org-slow-todo :wk "select todo")))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-leading-bullet ?\s))

(use-package org-agenda
  :after org)

;; enable <s, <n, etc...
;; doc: https://orgmode.org/manual/Structure-Templates.html
(use-package org-tempo
  :after org)

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme))

(use-package evil-org-agenda
  :after (evil-org org-agenda)
  :config
  (evil-org-agenda-set-keys))

(provide 'amber-org)

;;; amber-org.el ends here
