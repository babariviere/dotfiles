(define-module (baba home services wm)
  #:use-module (baba)
  #:use-module (gnu home services)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (stumpwm-service))

(define stump stumpwm+slynk)

(define stumpwm-service
  (list
   ;; NOTE: possible with .xsession since it's used by default with slim
   (simple-service 'stumpwm-xsession
		   home-files-service-type
		   `(("xprofile" ;; NOTE: move it into a service? to be able to extend it
		      ,(computed-file
			"xprofile"
			#~(begin
			    (use-modules (ice-9 format))
			    (with-output-to-file #$output
			      (lambda ()
				(format #t
					"#!/bin/sh~@
                                         if [ -e \"$HOME/.profile\" ]; then~@
                                           . \"$HOME/.profile\"~@
                                         fi~@
                                         ~a/bin/picom -b --config $HOME/.config/picom.conf"
					#$picom)))
			    (chmod #$output #o555))))
		     ("xsession"
		      ,(computed-file
			"xsession"
			#~(begin
			    (use-modules (ice-9 format))
			    (with-output-to-file #$output
			      (lambda ()
				(format #t
					"#!/bin/sh~@
                                         ~a/bin/stumpwm"
					#$stump)))
			    (chmod #$output #o555))))))
   (simple-service 'stumpwm-profile
		   home-profile-service-type
		   (list stump `(,stumpwm "lib")
			 sbcl-stumpwm-ttf-fonts
			 sbcl))
   (simple-service 'stumpwm-files
		   home-files-service-type
		   `(("stumpwm.d/init.lisp"
		      ,(local-file
			(string-append %channel-root "/etc/stumpwm.d/init.lisp")))))))
