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
  (let ((commit "0fb1fb30a2a5378d0cc7e0a085bd0c32f55517cd"))
    (package
     (inherit nyxt)
     (version (string-take commit 8))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atlas-engineer/nyxt")
             (commit commit)))
       (sha256
        (base32
         "1y4lx5ay89vhyig3nax7xwg4a8am6awkk8cylhmlja0f358f0y85"))
       (file-name (git-file-name "nyxt" version))))
     (inputs
      `(("cl-gopher" ,sbcl-cl-gopher)
        ("nhooks" ,sbcl-nhooks)
        ("phos" ,sbcl-phos)
        ("cl-tld" ,sbcl-cl-tld)
        ("nfiles" ,sbcl-nfiles)
        ,@(package-inputs nyxt))))))
