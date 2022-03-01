(define-module (baba home services browsers)
  #:use-module (baba)
  #:use-module (baba packages web-browsers)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages gstreamer)
  #:use-module (guix gexp)
  #:export (nyxt-service))

(define nyxt-service
  (list (simple-service 'nyxt-init
                        home-files-service-type
                        `(("config/nyxt/init.lisp"
                           ,(local-file (string-append %channel-root "/etc/nyxt/init.lisp")))))
        (simple-service 'nyxt-profile
                        home-profile-service-type
                        (list nyxt-next

                              ;; required for video (like youtube)
                              gst-plugins-bad
                              gst-plugins-base
                              gst-plugins-good
                              gst-plugins-ugly
                              gst-libav))))
