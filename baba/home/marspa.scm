(define-module (baba home marspa)
  #:use-module (baba home services emacs)
  #:use-module (baba home services shells)
  #:use-module (baba home services terminals)
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
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (flat packages emacs))

(define %sway-config
  `((include ,(file-append sway "/etc/sway/config"))
    ;; (bindsym $mod+Shift+e exec emacsclient -c --eval "(eshell)")
    (xwayland enable)
    (set $term  ,(file-append foot "/bin/foot"))
    (set $menu ,(file-append rofi "/bin/rofi -modi drun -show drun"))
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
  (append emacs-service
	  (list (service home-bash-service-type
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
			  (config %sway-config)))
		(service home-foot-service-type
			 (home-foot-configuration
			  (config
			   `((main
			      ((term . "xterm-256color")
			       (font . "MonoLisa:size=10")
			       (dpi-aware . "no")
			       ;; TODO: find a way to reference foot source
			       (include . ,(file-append (package-source foot) "/themes/dracula"))
			       ))))))))))
