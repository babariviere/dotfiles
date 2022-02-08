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
   (version "fdbc9373d33acc76820506fb4b263c7678cde25f")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/atlas-engineer/nyxt")
           (commit version)))
     (sha256
      (base32
       "1ykisvpvjz1s6yqxam3jllylsg8p6hlk42brcr4ri8vilkvijfnc"))
     (file-name (git-file-name "nyxt" version))))
   (inputs
    `(("cl-gopher" ,sbcl-cl-gopher)
      ("nhooks" ,sbcl-nhooks)
      ("phos" ,sbcl-phos)
      ("cl-tld" ,sbcl-cl-tld)
      ,@(package-inputs nyxt)))))
