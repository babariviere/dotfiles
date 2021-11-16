(define-module (baba home services emacs)
  #:use-module (baba)
  #:use-module (gnu packages emacs-xyz)
  #:use-module ((gnu packages emacs-xyz) #:prefix e:)
  #:use-module (emacs packages melpa)
  #:use-module (flat packages emacs)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:export (emacs-service))

(define rewrite-elisp-packages
  (package-input-rewriting
   `((,e:emacs-f . ,emacs-f)
     (,e:emacs-pkg-info . ,emacs-pkg-info)
     (,e:emacs-dash . ,emacs-dash)
     (,e:emacs-hydra . ,emacs-hydra)
     (,e:emacs-flycheck . ,emacs-flycheck)
     (,emacs-lsp-mode . ,e:emacs-lsp-mode)
     (,e:emacs-markdown-mode . ,emacs-markdown-mode)
     (,emacs-emacsql . ,e:emacs-emacsql))))

(define %elisp-packages
  (map rewrite-elisp-packages
       (list
	;; emacs-doom-modeline
	emacs-helpful
	emacs-use-package
	emacs-which-key
	emacs-gcmh
	e:emacs-hl-todo
	emacs-git-gutter
	emacs-git-gutter-fringe
	emacs-exec-path-from-shell
	emacs-vertico
	emacs-marginalia
	emacs-orderless
	emacs-consult
	emacs-embark
	;; emacs-embark-consult

	emacs-projectile

	;; emacs-evil
	;; emacs-evil-collection
	;; emacs-evil-surround
	;; emacs-evil-commentary
	emacs-general
	emacs-undo-fu
	emacs-undo-fu-session
	emacs-eldoc

	e:emacs-magit
	e:emacs-forge

	emacs-diredfl
	;; emacs-all-the-icons-dired
	emacs-diff-hl

	emacs-sly
	;; emacs-sly-macrostep
	;; emacs-sly-repl-ansi-color

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

	emacs-elixir-mode
	emacs-alchemist
	emacs-exunit

	emacs-fish-mode

	emacs-flycheck
	emacs-flycheck-inline

	;; emacs-format-all

	emacs-go-mode
	emacs-go-guru

	e:emacs-lsp-mode
	e:emacs-lsp-ui
	;; emacs-consult-lsp

	emacs-org
	emacs-org-appear
	emacs-org-contrib
	emacs-org-edna
	emacs-org-superstar
	;; emacs-evil-org
	emacs-org-roam

	;; emacs-nix-mode
	;; emacs-nix-update

	;; emacs-perspective
	;; emacs-persp-projectile

	;; emacs-python-mode
	;; emacs-poetry
	;; emacs-lsp-python-ms

	emacs-smartparens

	e:emacs-yasnippet
	emacs-doom-snippets

	emacs-doom-themes
	emacs-kaolin-themes
	emacs-modus-themes
	emacs-inkpot-theme

	emacs-vterm

	e:emacs-geiser
	e:emacs-guix

	emacs-emacsql-sqlite3
	sqlite
	gcc
	emacs-editorconfig
	emacs-git-auto-commit-mode
	emacs-zoom
	emacs-pinentry
	emacs-crux
	emacs-htmlize

	emacs-avy
	emacs-ace-window
	)))

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
		  (package emacs-pgtk-native-comp)
		  (elisp-packages %elisp-packages)
		  (server-mode? #t)
		  (rebuild-elisp-packages? #f)
		  (xdg-flavor? #t)))
	(simple-service 'emacs-init
			home-files-service-type
			(emacs-files))))
