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
   (version "37000e6cae435ec7453f3d3318afe9d588a546bf")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/atlas-engineer/nyxt")
           (commit version)))
     (sha256
      (base32
       "0jv32z887cya4qymg28ggi91bh06hrsis2iwl637ad5ixh4x4bwv"))
     (file-name (git-file-name "nyxt" version))))
   (inputs
    `(("cl-gopher" ,sbcl-cl-gopher)
      ("nhooks" ,sbcl-nhooks)
      ("phos" ,sbcl-phos)
      ,@(package-inputs nyxt)))))
