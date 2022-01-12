(define-module (baba home services emacs)
  #:use-module (baba)
  #:use-module (gnu packages emacs-xyz)
  #:use-module ((gnu packages emacs-xyz) #:prefix e:)
  #:use-module (emacs build-system melpa)
  #:use-module (emacs packages melpa)
  #:use-module (flat packages emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (emacs-service))

(define rewrite-elisp-packages
  (package-input-rewriting/spec
   `(("emacs-f" . ,(const emacs-f))
     ("emacs-s" . ,(const emacs-s))
     ("emacs-pkg-info" . ,(const emacs-pkg-info))
     ("emacs-dash" . ,(const emacs-dash))
     ("emacs-hydra" . ,(const emacs-hydra))
     ("emacs-flycheck" . ,(const emacs-flycheck))
     ("emacs-lsp-mode" . ,(const emacs-lsp-mode))
     ("emacs-markdown-mode" . ,(const emacs-markdown-mode))
     ("emacs-emacsql" . ,(const emacs-emacsql))
     ("emacs-emacsql-sqlite" . ,(const emacs-emacsql-sqlite))
     ("emacs-yasnippet" . ,(const emacs-yasnippet))
     ("emacs-cider" . ,(const emacs-cider))
     ("emacs-org-contrib" . ,(const emacs-org-contrib)))))

(define %native-emacs emacs-native-comp)

(define (update-emacs-argument-for-package p)
  (if (equal?
       (package-build-system p)
       melpa-build-system)
      (package (inherit p)
	       (arguments
		(substitute-keyword-arguments (package-arguments p)
					      ((#:emacs e #f) %native-emacs))))
      p))

(define use-native-emacs
  (package-mapping update-emacs-argument-for-package
		   (lambda (p) #f)))

(define %elisp-packages
  (map rewrite-elisp-packages
       (map use-native-emacs
	    (list
	     ;; emacs-doom-modeline
	     emacs-helpful
	     emacs-use-package
	     emacs-which-key
	     emacs-gcmh
	     emacs-hl-todo
	     emacs-git-gutter
	     emacs-git-gutter-fringe
	     emacs-exec-path-from-shell
	     emacs-vertico
	     emacs-marginalia
	     emacs-orderless
	     emacs-consult
	     emacs-embark
	     emacs-embark-consult

	     emacs-projectile

	     ;; emacs-evil
	     ;; emacs-evil-collection
	     ;; emacs-evil-surround
	     ;; emacs-evil-commentary
	     emacs-general
	     emacs-undo-fu
	     emacs-undo-fu-session
	     emacs-eldoc

	     emacs-magit
	     emacs-forge

	     emacs-diredfl
	     emacs-all-the-icons-dired
	     emacs-diff-hl

	     emacs-sly
	     emacs-sly-macrostep
	     emacs-sly-repl-ansi-color

	     emacs-lispy
	     ;; emacs-lispyville

	     emacs-ccls
	     emacs-cmake-mode
	     emacs-demangle-mode
	     emacs-disaster
	     emacs-modern-cpp-font-lock

	     emacs-company
	     emacs-company-box

	     emacs-csharp-mode

	     emacs-yaml-mode
	     emacs-gitlab-ci-mode

	     emacs-envrc

	     emacs-eros
	     emacs-highlight-quoted
	     emacs-elisp-demos
	     emacs-macrostep
	     emacs-macrostep-geiser

	     ;; emacs-elixir-mode
	     ;; emacs-alchemist
	     ;; emacs-exunit

	     emacs-fish-mode

	     emacs-flycheck
	     emacs-flycheck-inline

	     ;; emacs-format-all

	     emacs-go-mode
	     emacs-go-guru

	     emacs-lsp-mode
	     emacs-lsp-ui
	     emacs-consult-lsp

	     emacs-org
	     emacs-org-appear
	     emacs-org-contrib
	     emacs-org-edna
	     emacs-org-superstar
	     ;; emacs-evil-org
	     emacs-org-roam

	     emacs-nix-mode
	     emacs-nix-update

	     ;; emacs-perspective
	     ;; emacs-persp-projectile

	     emacs-python-mode
	     emacs-poetry
	     emacs-lsp-python-ms

	     emacs-smartparens

	     emacs-yasnippet
	     emacs-doom-snippets

	     emacs-doom-themes
	     emacs-kaolin-themes
	     emacs-modus-themes
	     emacs-inkpot-theme

	     emacs-vterm

	     emacs-geiser
	     emacs-guix
	     emacs-geiser-guile

	     emacs-emacsql-sqlite3
	     sqlite
	     gcc
	     emacs-editorconfig
	     emacs-git-auto-commit-mode
	     emacs-zoom
	     ;; emacs-pinentry
	     emacs-crux
	     emacs-htmlize

	     emacs-avy
	     emacs-ace-window

         emacs-verb
	     ))))

;; TODO: migrate this to a ~home-config-files-service-type~
(define (emacs-files)
  (define config-root
    (string-append %channel-root "/etc"))
  (define emacs-root
    (string-append config-root "/emacs.d"))

  (define (enter? name stat result)
    #t)
  (define (leaf name stat result)
    (cons `(,(string-drop name (+ (string-length config-root) 1)) ,(local-file name)) result))

  (define (down name stat result) result)
  (define (up name stat result) result)

  (define (skip name stat result) result)
  (define (error name stat errno result) result)

  (file-system-fold enter? leaf down up skip error
		    '()
		    emacs-root))


(define emacs-service
  (list (service home-emacs-service-type
		 (home-emacs-configuration
		  (package %native-emacs)
		  (elisp-packages %elisp-packages)
		  (server-mode? #t)
		  (rebuild-elisp-packages? #t)
		  (xdg-flavor? #t)))
	(simple-service 'emacs-init
			home-files-service-type
			(emacs-files))
	(simple-service 'emacs-org-protocol
			home-xdg-mime-applications-service-type
			(home-xdg-mime-applications-configuration
			 (added '((x-scheme-handler/org-protocol . org-protocol.desktop)))
			 (desktop-entries
			  (list
			   (xdg-desktop-entry
			    (file "org-protocol")
			    (name "Org Protocol")
			    (type 'application)
			    (config
			     '((exec . "emacsclient %u"))))))))))