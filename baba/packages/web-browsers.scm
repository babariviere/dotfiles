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
  (let ((commit "c89c0c5d8595426c71fee52a62ed4e86b2390a37"))
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
         "1j81zr9g6jc4if27r0lqbcp29yi2ni86xbslcpqfc6jpmd6ffrjj"))
       (file-name (git-file-name "nyxt" version))))
     (inputs
      `(("cl-gopher" ,sbcl-cl-gopher)
        ("nhooks" ,sbcl-nhooks)
        ("phos" ,sbcl-phos)
        ("cl-tld" ,sbcl-cl-tld)
        ,@(package-inputs nyxt))))))
