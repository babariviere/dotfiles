(define-module (baba packages web-browsers)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages webkit)
  #:use-module (guix build-system asdf)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public nyxt-next
  (package
   (inherit nyxt)
   (version "76fecd19103e1a01057a22e31306e0cca68aa102")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/atlas-engineer/nyxt")
           (commit version)))
     (sha256
      (base32
       "131pv80lxaf36w1fhywwc4l14v8iig7v4mvyq97skpmjbswx3c9f"))
     (file-name (git-file-name "nyxt" version))))
   (inputs
    `(("cl-gopher" ,sbcl-cl-gopher)
      ("nhooks" ,sbcl-nhooks)
      ("phos" ,sbcl-phos)
      ,@(package-inputs nyxt)))))
