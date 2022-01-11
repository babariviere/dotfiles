(define-module (baba home services wm)
  #:use-module (baba)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (stumpwm-service))

(define stumpwm-service
  (list
   ;; TODO: how to put them here?
   ;; (simple-service 'stumpwm-profile
   ;; 		   home-profile-service-type
   ;; 		   (list stumpwm `(,stumpwm "lib")
   ;; 			 sbcl-stumpwm-ttf-fonts
   ;; 			 sbcl
   ;; 			 sbcl-slynk))
   (simple-service 'stumpwm-files
		   home-files-service-type
		   `(("stumpwm.d/init.lisp"
		      ,(local-file
			(string-append %channel-root "/etc/stumpwm.d/init.lisp")))))))
