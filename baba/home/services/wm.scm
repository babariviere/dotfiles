(define-module (baba home services wm)
  #:use-module (baba)
  #:use-module (baba packages wm)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (stumpwm-service))

(define stump stumpwm+slynk)

(define stumpwm-service
  (list
   (simple-service 'stumpwm-xsession
		           home-files-service-type
		           `(("xsession"
		              ,(computed-file
			            "xsession"
			            #~(begin
			                (use-modules (ice-9 format))
			                (with-output-to-file #$output
			                  (lambda ()
				                (format #t
					                    "~
#!/bin/sh

export GDK_CORE_DEVICE_EVENTS=1

if [ -e \"$HOME/.profile\" ]; then
  . \"$HOME/.profile\"
fi

if test -z \"$DBUS_SESSION_BUS_ADDRESS\"; then
  eval `dbus-launch --sh-syntax`
fi

~a/bin/picom -b --config $HOME/.config/picom.conf --experimental-backends
~a/bin/stumpwm"
                                        #$picom
					                    #$stump)))
			                (chmod #$output #o555))))
                     ("config/picom.conf" ,(local-file (string-append %channel-root "/etc/picom.conf")))))
   (simple-service 'setup-sbcl
                   home-shell-profile-service-type
                   (list (plain-file "setup-sbcl"
                                     "export SBCL_HOME=\"$HOME/.guix-home/profile/lib/sbcl\"")))
   (simple-service 'stumpwm-profile
		           home-profile-service-type
		           (list stump `(,stumpwm "lib")
                         sbcl-stumpwm-battery-portable
                         sbcl-stumpwm-cpu
                         sbcl-stumpwm-mem
                         sbcl-stumpwm-net
                         sbcl-stumpwm-notify
                         sbcl-stumpwm-stumptray
                         sbcl-stumpwm-ttf-fonts
                         sbcl-stumpwm-wifi
                         sbcl

                         ;; tools
                         alacritty
                         autorandr
                         picom
                         pamixer
                         flameshot

                         ;; hardware acceleration
                         mesa
                         gstreamer
                         gst-plugins-bad
                         gst-plugins-base
                         gst-plugins-good
                         gst-plugins-ugly
                         gst-libav
                         intel-vaapi-driver
                         libva-utils
                         ))
   (simple-service 'stumpwm-files
		           home-files-service-type
		           `(("stumpwm.d/init.lisp"
		              ,(local-file
			            (string-append %channel-root "/etc/stumpwm.d/init.lisp")))))))
