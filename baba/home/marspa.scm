(define-module (baba home marspa)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (baba packages emacs-xyz)
  #:use-module (flat packages emacs))

;; (define emacs-dash-patched
;;   (package
;;     (inherit emacs-dash)
;;     (arguments
;;       `(#:phases
;; 	(modify-phases %standard-phases
;; 	  (add-after 'unpack 'modify-makefile
;; 	    (lambda* (#:key outputs #:allow-other-keys)
;; 		     (substitute* "Makefile"
;; 			(("byte-compile-error-on-warn t")
;; 			 "byte-compile-error-on-warn nil"))
;; 		     #t)))))))
;; 
;; (define replace-broken-packages
;;   (package-input-rewriting `((,emacs-dash . ,emacs-dash-patched))) #:deep? #f)

(define %elisp-packages
  (list
   emacs-doom-modeline
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
   ;; emacs-embark-consult
   
   emacs-projectile
   
   emacs-evil
   emacs-evil-collection
   emacs-evil-surround
   emacs-evil-commentary
   emacs-general
   emacs-undo-fu
   emacs-undo-fu-session
   emacs-eldoc
   
   emacs-magit
   emacs-magit-todos
   emacs-forge
   
   emacs-diredfl
   emacs-all-the-icons-dired
   emacs-diff-hl
   
   emacs-sly
   emacs-sly-macrostep
   emacs-sly-repl-ansi-color
   
   emacs-lispy
   emacs-lispyville
   
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
   
   emacs-lsp-mode
   emacs-lsp-ui
   emacs-consult-lsp
   
   emacs-org
   emacs-org-appear
   emacs-org-contrib
   emacs-org-edna
   emacs-org-superstar
   emacs-evil-org
   emacs-org-roam
   
   emacs-nix-mode
   emacs-nix-update
   
   ;; emacs-perspective
   ;; emacs-persp-projectile
   
   ;; emacs-python-mode
   emacs-poetry
   emacs-lsp-python-ms
   
   emacs-smartparens
   
   emacs-yasnippet
   emacs-doom-snippets
   
   emacs-doom-themes
   emacs-kaolin-themes
   emacs-modus-themes
   
   emacs-vterm

   emacs-geiser
   emacs-guix
   
   emacs-emacsql-sqlite3
   sqlite
   gcc
   emacs-editorconfig
   emacs-git-auto-commit-mode
   emacs-zoom
   ;; emacs-pinentry
   ))

(define %sway-config
  `((include ,(file-append sway "/etc/sway/config"))
    ;; (bindsym $mod+Shift+e exec emacsclient -c --eval "(eshell)")
    (input * ((xkb_layout "us")
	      (xkb_variant "altgr-intl")
	      (xkb_options "ctrl:nocaps")))
    (output HDMI-A-1 res 2560x1440@60Hz pos 0 0)
    (output DP-2 res 2560x1440@60Hz pos 2560 0)
    (output eDP-1 pos ,(inexact->exact (/ 3840 2)) 1440)
    (bindswitch lid:on output eDP-1 disable)
    (bindswitch lid:off output eDP-1 enable)
    (focus_follows_mouse no)))

(home-environment
 (packages
  (list htop))
 (services
  (list (service home-emacs-service-type
		 (home-emacs-configuration
		  (package emacs-pgtk-native-comp)
		  (elisp-packages %elisp-packages)
		  (server-mode? #t)
		  (rebuild-elisp-packages? #f)
		  (xdg-flavor? #t)))
	(service home-bash-service-type
		 (home-bash-configuration
		  (guix-defaults? #t)))
	(service home-fish-service-type
		 (home-fish-configuration
		  (abbreviations '(("gco" . "git checkout")
				   ("gs" . "git status")
				   ("gsr" . "sudo -E guix system reconfigure")
				   ("ghr" . "guix home reconfigure")))))
	(service home-gnupg-service-type
		 (home-gnupg-configuration
		   (gpg-agent-config
		     (home-gpg-agent-configuration
		       ;; (pinentry-flavor 'emacs)
		       (extra-config
			 '((max-cache-ttl . 86400)))
		       ))))
	(service home-git-service-type
		 (home-git-configuration
		   (config
		     `((user
			 ((name . "Bastien Riviere")
			  (email . "babathriviere@gmail.com")
			  (signingKey . "F9B7864F2AB46F18")))
		       (github
			 ((user . "babariviere")))
		       (remote
			 ((pushDefault . "origin")))
		       (commit
			 ((gpgSign . #t)))))))
	(service home-sway-service-type
		 (home-sway-configuration
		  (config %sway-config))))))

