(define-module (baba home services emacs)
  #:use-module (baba)
  #:use-module (baba packages emacs)
  #:use-module (baba packages fonts)
  #:use-module (gnu packages emacs-xyz)
  #:use-module ((gnu packages emacs-xyz) #:prefix e:)
  #:use-module (emacs build-system melpa)
  #:use-module (emacs packages elpa)
  #:use-module (emacs packages melpa)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages uml)
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
     ("emacs-markdown-mode" . ,(const emacs-markdown-mode))
     ("emacs-emacsql" . ,(const emacs-emacsql))
     ("emacs-emacsql-sqlite" . ,(const emacs-emacsql-sqlite))
     ("emacs-cider" . ,(const emacs-cider))
     ("emacs-org-contrib" . ,(const emacs-org-contrib))
     ("emacs-ht" . ,(const emacs-ht)))))

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
       (list
	    emacs-helpful
	    emacs-use-package
	    emacs-which-key
	    emacs-gcmh
	    emacs-hl-todo
	    emacs-git-gutter
	    emacs-git-gutter-fringe
	    emacs-exec-path-from-shell

        ;; completion
	    emacs-vertico
	    emacs-marginalia
	    emacs-orderless
	    emacs-consult
	    emacs-embark
	    emacs-embark-consult

        emacs-general
	    emacs-undo-fu
	    emacs-undo-fu-session
        emacs-vundo
	    emacs-eldoc

        ;; magit
	    emacs-magit
        emacs-magit-section
	    emacs-forge

        ;; dired
	    emacs-diredfl
	    emacs-all-the-icons-dired
	    emacs-diff-hl

        ;; common lisp
	    emacs-sly
        emacs-sly-asdf
	    emacs-sly-macrostep
	    emacs-sly-repl-ansi-color

        ;; lisp
	    emacs-lispy

        ;; c/c++
	    emacs-ccls
	    emacs-cmake-mode
	    emacs-demangle-mode
	    emacs-disaster
	    emacs-modern-cpp-font-lock

        ;; check
        emacs-flymake-diagnostic-at-point

        ;; corfu
        emacs-corfu
        emacs-cape
        emacs-kind-icon
        emacs-corfu-doc
        emacs-pcmpl-args

        ;; snippets
        emacs-tempel
        emacs-yasnippet

        ;; csharp
	    emacs-csharp-mode
        emacs-csproj-mode
        emacs-sharper

        ;; data
        emacs-json-mode
        emacs-yaml-mode
	    emacs-gitlab-ci-mode

        ;; direnv
	    emacs-envrc

        ;; docker
        emacs-docker
        emacs-dockerfile-mode

        ;; emacs lisp
	    emacs-eros
	    emacs-highlight-quoted
	    emacs-elisp-demos
	    emacs-macrostep
	    emacs-macrostep-geiser

        ;; erlang
        emacs-erlang

        ;; elixir
        emacs-elixir-mode
        emacs-alchemist
        emacs-exunit

        ;; fish
	    emacs-fish-mode

        ;; go
	    emacs-go-mode
	    emacs-go-guru

        ;; haskell
        emacs-haskell-mode
        emacs-hindent

        ;; lsp
	    emacs-eglot

        ;; elfeed
        emacs-elfeed
        emacs-elfeed-org

        ;; org-mode
	    emacs-org
	    emacs-org-appear
	    emacs-org-contrib
	    emacs-org-edna
	    emacs-org-superstar
	    emacs-org-roam
        emacs-org-roam-ui

        ;; nix
	    emacs-nix-mode
	    emacs-nix-update

        ;; python
        emacs-python-mode
	    emacs-poetry
        emacs-blacken

        ;; parens
	    emacs-smartparens

        ;; themes
	    emacs-modus-themes

        ;; shell
	    emacs-vterm

        ;; scheme
	    emacs-geiser
	    emacs-guix
	    emacs-geiser-guile

        ;; misc
	    emacs-emacsql-sqlite3
	    sqlite
	    gcc
	    emacs-editorconfig
	    emacs-git-auto-commit-mode
	    emacs-zoom
	    emacs-crux
	    emacs-htmlize
        emacs-diminish

        ;; navigation
	    emacs-avy
	    emacs-ace-window

        ;; http
        emacs-verb

        ;; services
        emacs-slack
        emacs-notmuch

        ;; dependencies
        python-pygments
        emacs-plantuml-mode
        plantuml
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
    ;; Trim %channel-root/etc/ from the path and prefix it with .
    (cons `(,(string-append "." (string-drop name (+ (string-length config-root) 1))) ,(local-file name)) result))

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
		          (rebuild-elisp-packages? #f)
		          (xdg-flavor? #t)))
	    (simple-service 'emacs-init
			            home-files-service-type
			            (emacs-files))
        (simple-service 'emacs-fonts
                        home-profile-service-type
                        (list font-fira-go font-iosevka font-iosevka-aile))
        ;; move firefox protocol
	    (simple-service 'emacs-org-protocol
			            home-xdg-mime-applications-service-type
			            (home-xdg-mime-applications-configuration
			             (added '((x-scheme-handler/org-protocol . org-protocol.desktop)))
                         (default '((text/html . org.mozilla.firefox.desktop)
                                    (x-scheme-handler/http . org.mozilla.firefox.desktop)
                                    (x-scheme-handler/https . org.mozilla.firefox.desktop)
                                    (x-scheme-handler/about . org.mozilla.firefox.desktop)
                                    (x-scheme-handler/unknown . org.mozilla.firefox.desktop)))
                         (desktop-entries
			              (list
			               (xdg-desktop-entry
			                (file "org-protocol")
			                (name "Org Protocol")
			                (type 'application)
			                (config
			                 '((exec . "emacsclient %u"))))))))))
