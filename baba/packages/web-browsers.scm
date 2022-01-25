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
   (version "dev")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/atlas-engineer/nyxt")
           (commit "2d678d04bd9bb45a80f6caf2ca1056719698313e")))
     (sha256
      (base32
       "0pqc4sh8ff1kb7jszk6l710h4rdna0acyk466rd09fy8vf2rhgrv"))
     (file-name (git-file-name "nyxt" version))))
   (inputs
    `(("cl-gopher" ,sbcl-cl-gopher)
      ("nhooks" ,sbcl-nhooks)
      ("phos" ,sbcl-phos)
      ,@(package-inputs nyxt)))))
