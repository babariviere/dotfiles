(define-module (baba packages wm)
  #:use-module (gnu packages wm)
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
