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
  (let ((commit "75977e76571d1a0075d5bfb57c8748b8627720fa"))
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
         "10f3pk2h05xfa080zxxqqbvmndjl6izhlvb6myjj61fm1046dwbb"))
       (file-name (git-file-name "nyxt" version))))
     (inputs
      `(("cl-gopher" ,sbcl-cl-gopher)
        ("nhooks" ,sbcl-nhooks)
        ("phos" ,sbcl-phos)
        ("cl-tld" ,sbcl-cl-tld)
        ("nfiles" ,sbcl-nfiles)
        ,@(package-inputs nyxt))))))
