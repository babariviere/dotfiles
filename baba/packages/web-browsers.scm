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
  (let ((commit "d5142d8338b861ad0bfd94e1baf9a5de0101c799")
        (revision "2"))
    (package
     (inherit nyxt)
     (version (git-version "3" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atlas-engineer/nyxt")
             (commit commit)))
       (sha256
        (base32
         "0p4n37iynn42p0i0chn4l4na667jjbam1d7rhn5sd79l3lnfibq2"))
       (file-name (git-file-name "nyxt" version))))
     (inputs
      `(("cl-gopher" ,sbcl-cl-gopher)
        ("nhooks" ,sbcl-nhooks)
        ("phos" ,sbcl-phos)
        ("cl-tld" ,sbcl-cl-tld)
        ("nfiles" ,sbcl-nfiles)
        ,@(package-inputs nyxt))))))
