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

;;
;; Functions
;;

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

(defun amber/org-src-fix-newline-and-indent-a (&optional indent _arg _interactive)
  "Mimic `newline-and-indent` in src blocks."
  (when (and indent
			 org-src-tab-acts-natively
			 (org-in-src-block-p t))
    (org-babel-do-in-edit-buffer
     (call-interactively #'indent-for-tab-command))))

(defun amber/org-archive-subtree-as-completed ()
  "Archive subtree into the daily file and mark it as completed if not done."
  (interactive)
  (let ((todo (org-get-todo-state))
		(org-after-todo-state-change-hook '()))
    (when (not (or (equal "DONE" todo)
				   (equal "MEETING" todo)
				   (equal "CANCELLED" todo)))
      (org-todo "DONE")))

  (let* ((org-refile-keep nil)
		 (org-roam-dailies-capture-templates
		  '(("d" "default" entry "%?" :target
			 (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
		 (org-after-refile-insert-hook #'save-buffer)
		 today-file
		 pos)
	(save-window-excursion
	  (org-roam-dailies--capture (current-time) t)
	  (setq today-file (buffer-file-name))
	  (setq pos (point)))

	(unless (equal (file-truename today-file)
				   (file-truename (buffer-file-name)))
	  (org-refile nil nil (list "Tasks" today-file nil pos)))))

(defun amber/org-archive-if-done ()
  "Archive task if it's marked as done."
  (when (equal (org-get-todo-state) "DONE")
	(amber/org-archive-subtree-as-completed)))

(defun amber/org-clean-refile-tag ()
  "Remove refile tag from refiled item."
  (save-window-excursion
    (org-refile-goto-last-stored)
    (org-set-tags
     (remove "refile"
			 (seq-remove (lambda (tag) (get-text-property 0 'inherited tag))
						 (org-get-tags))))))

(defun amber/org-goto-inbox ()
  "Goto inbox file."
  (interactive)
  (find-file (expand-file-name org-inbox-file org-directory)))

(defun amber/org-goto-agenda ()
  "Goto agenda file."
  (interactive)
  (find-file (expand-file-name org-agenda-file org-directory)))

(defun amber/org-goto-tasks ()
  "Goto tasks file."
  (interactive)
  (find-file (expand-file-name org-tasks-file org-directory)))

(defun amber/org-roam-toggle-buffer ()
  "Toggle org-roam buffer if not visible."
  (and (not org-roam-capture--node)
       (not (eq 'visible (org-roam-buffer--visibility)))
       (org-roam-buffer-toggle)))

;;
;; Variables
;;

(defgroup amber-org nil "Customization for amber's org flow.")

(defcustom org-inbox-file "inbox.org"
  "File to use for inbox."
  :type 'file
  :group 'amber-org)

(defcustom org-agenda-file "agenda.org"
  "File to use for agenda.
Contains non-actionnable tasks and/or tasks related to people.
Examples:
- a meeting
- a recuring event (haircut, cleaning, ...)
- call someone"
  :type 'file
  :group 'amber-org)

(defcustom org-tasks-file "tasks.org"
  "File to use for all actionnable tasks.  They are mostly programming tasks.
Examples:
- work on project task
- customize Emacs org mode
- do a PR review"
  :type 'file
  :group 'amber-org)

(defcustom org-meeting-directory "meeting/"
  "Directory holding all past meetings."
  :type 'directory
  :group 'amber-org)

(defcustom org-reference-directory "refs/"
  "Directory holding all web references."
  :type 'directory
  :group 'amber-org)

;;
;; Packages setup
;;

;; TODO: to handle meeting:
;; when meeting done, move subtree to meeting/%{date}-%{slug}

(use-package org
  :hook ((org-mode . visual-line-mode)
		 (org-mode . amber/org-babel-lazy-load-h)
		 (org-after-todo-state-change . amber/org-archive-if-done))
  :demand t
  :custom
  (org-hide-emphasis-marker t)
  (org-hide-leading-star nil)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-startup-indented t)
  (org-confirm-babel-evaluate nil)
  (org-edit-src-content-indentation 0)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-directory (expand-file-name "~/src/github.com/babariviere/notes/"))
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)")
					   (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING(m)")))
  ;; TODO: better colors
  (org-todo-keyword-faces '(("TODO" . org-todo)
							("NEXT" . org-warning)
							("DONE" . org-done)
							("WAITING" . org-warning)
							("HOLD" . org-warning)
							("CANCELLED" . org-archived)
							("MEETING" . org-warning)))
  (org-global-properties '(("Effort_ALL" . "0:05 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
  :config
  (advice-add #'org-return :after #'amber/org-src-fix-newline-and-indent-a)
  :general
  ('normal org-mode-map
  		   "RET" #'amber/org-dwin-at-point
		   [ret] #'amber/org-dwin-at-point)
  (amber/local-leader-keys org-mode-map
	"" nil
    "a" '(:ignore t :wk "archive")
    "at" '(amber/org-archive-subtree-as-completed :wk "archive task")
    "c" '(:ignore t :wk "clock")
    "ci" '(org-clock-in :wk "clock in")
    "co" '(org-clock-out :wk "clock out")
    "e" '(org-set-effort :wk "set effort")
	"i" '(org-roam-node-insert :wk "insert node")
	"l" '(org-insert-link :wk "insert link")
    "p" '(org-priority :wk "set priority")
    "t" '(amber/org-slow-todo :wk "select todo"))
  (amber/leader-keys
    "n" '(:ignore t :wk "notes")
    "nc" '(org-capture :wk "capture")
    "oA" '(amber/org-goto-agenda :wk "open agenda.org")
    "oi" '(amber/org-goto-inbox :wk "open inbox.org")
    "oI" '(amber/org-goto-tasks :wk "open tasks.org")))

(use-package org-capture
  :after org
  :custom
  ;; TODO: template must be able to accept parameters from org-roam files
  ;; Examples:
  ;; - you can add people to meetings
  ;;
  (org-capture-templates
   `(("i" "Inbox" entry (file ,(concat org-directory org-inbox-file))
      "* %? :refile:\n:PROPERTIES:\n:CREATED: %U\n:END:"
      :clock-in t :clock-resume t :empty-lines 1)
     ("t" "Task" entry (file ,(concat org-directory org-inbox-file))
      "* TODO %? :refile:\n:PROPERTIES:\n:CREATED: %U\n:END:"
      :clock-in t :clock-resume t :empty-lines 1)
     ("m" "Meeting (work)" entry (file ,(concat org-directory org-agenda-file))
      "* MEETING [%<%Y-%m-%d %a>] :meeting:work:refile:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\nParticipants:%^{Participants}\nNotes:\n%?"
      :clock-in t :clock-resume t :empty-lines 1)
     ("p" "PR Review" entry (file+headline ,(concat org-directory org-tasks-file) "Pull requests")
      "* REVIEW gh:%?\n:PROPERTIES:\n:CREATED: %U\n:END:"))))

;; TODO: define org-link gh:owner/repo#pr + define org-protocol to handle it

(use-package org-protocol
  :after org)

(use-package org-agenda
  :after org
  :custom
  (org-agenda-files (mapcar (-partial #'concat org-directory)
							(list org-inbox-file org-agenda-file org-tasks-file)))
  :general
  (amber/leader-keys
    "oa" '(org-agenda :wk "open agenda")))

;; TODO: use hydra for refiling as in http://www.howardism.org/Technical/Emacs/getting-more-boxes-done.html
(use-package org-refile
  :after org
  :hook (org-after-refile-insert . amber/org-clean-refile-tag)
  :custom
  (org-refile-targets '((nil :maxlevel . 3)
						(org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  :general
  (amber/local-leader-keys org-mode-map
    "r" '(org-refile :wk "refile")))

(use-package org-clock
  :after org
  :custom
  (org-clock-out-remove-zero-time-clocks t))

;; Allows for trigger and blocker
;; See: https://www.nongnu.org/org-edna-el/
(use-package org-edna
  :after org
  :hook (org-mode . org-edna-mode))

(use-package org-roam
  :demand t
  :hook ((org-roam-mode . org-roam-db-autosync-mode)
		 (org-roam-find-file . amber/org-roam-toggle-buffer))
  :custom
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-directory org-directory)
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "brain/${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :init
  (setq org-roam-v2-ack t)

  :config
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
				 (display-buffer-in-side-window)
				 (side . right)
				 (slot . 0)
				 (window-width . 0.33)
				 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))

  :general
  (amber/leader-keys
    "nf" '(org-roam-node-find :wk "find note")))

(use-package org-roam-dailies
  :after org-roam
  :custom
  (org-roam-dailies-directory "journal/"))

(use-package org-roam-protocol
  :after org-roam
  :custom
  (org-roam-capture-ref-templates
   '(("r" "Reference" plain
      "%?"
      :target (file+head "refs/${slug}.org"
						 "#+TITLE: ${title}\n#+ROAM_REFS: ${refs}\n\n")
      :unnarrowed t)))
  :init
  (make-directory (concat org-roam-directory org-reference-directory) t))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t)
  (org-appear-autoemphasis t))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-leading-bullet ?\s))

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
