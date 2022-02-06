(define-module (baba home geras)
  #:use-module (baba home services emacs)
  #:use-module (baba home services wm)
  #:use-module (baba packages security)
  #:use-module (baba packages web-browsers)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu services)
  #:use-module (guix gexp))

;; TODO: extend service to source home-manager variables.
(home-environment
 (packages
  (list shepherd nyxt-next

        ;; required for video (like youtube)
        gst-plugins-bad
        gst-plugins-base
        gst-plugins-good
        gst-plugins-ugly
        gst-libav

        keychain))
 (services
  (append emacs-service
          stumpwm-service
          (list
           (simple-service 'setup-nix
                           home-shell-profile-service-type
                           (list (plain-file "setup-nix"
											 ". $HOME/.profile-nix")))))))
