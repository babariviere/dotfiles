(define-module (baba packages web-browsers)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages webkit)
  #:use-module (guix git-download)
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
           (commit "bdf8e4cdf9628a349e5c618016c5b06cd14b96e6")))
     (sha256
      (base32
       "1hw98vysi3gl7pc6lfj54h6zr6rgbn89id8ab390nhid8nrffjbz"))
     (file-name (git-file-name "nyxt" version))))
   (inputs
    `(("cl-gopher" ,sbcl-cl-gopher)
      ,@(package-inputs nyxt)))))
