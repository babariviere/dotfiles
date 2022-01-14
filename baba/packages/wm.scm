(define-module (baba packages wm)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages wm)
  #:use-module (guix build-system asdf)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public sbcl-stumpwm-battery-portable
  (package
   (inherit (@@ (gnu packages wm) stumpwm-contrib))
   (name "sbcl-stumpwm-battery-portable")
   (inputs
    `((,stumpwm "lib")))
   (arguments
    '(#:asd-systems '("battery-portable")
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'chdir (lambda _ (chdir "modeline/battery-portable") #t)))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib")
   (synopsis "Add battery information to the modeline in a portable way.")
   (description "Modeline support for battery.")
   (license license:gpl3+)))

(define sbcl-xml-emitter
  (package
   (name "sbcl-xml-emitter")
   (version "1.1.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/VitoVan/xml-emitter")
           (commit "1a93a5ab084a10f3b527db3043bd0ba5868404bf")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1w9yx8gc4imimvjqkhq8yzpg3kjrp2y37rjix5c1lnz4s7bxvhk9"))))
   (build-system asdf-build-system/sbcl)
   (inputs
    (list sbcl-cl-utilities))
   (arguments
    `(#:asd-systems '("xml-emitter")
      #:tests? #f))
   (home-page "https://github.com/VitoVan/xml-emitter")
   (synopsis "xml-emitter simply emits XML")
   (description "xml-emitter simply emits XML, with some
complexity for handling indentation. It can be used to produce all
sorts of useful XML output; it has an RSS 2.0 emitter built in.")
   (license license:expat)))

(define-public sbcl-stumpwm-notify
  (package
   (inherit (@@ (gnu packages wm) stumpwm-contrib))
   (name "sbcl-stumpwm-notify")
   (inputs
    `((,stumpwm "lib")
      ,sbcl-bordeaux-threads
      ,sbcl-dbus
      ,sbcl-xml-emitter))
   (arguments
    '(#:asd-systems '("notify")
      #:tests? #f
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'chdir (lambda _ (chdir "util/notify") #t)))))
   (home-page "https://github.com/stumpwm/stumpwm-contrib")
   (synopsis "DBus-based notification server part.")
   (description "Notification support for stumpwm.")
   (license license:gpl3+)))
