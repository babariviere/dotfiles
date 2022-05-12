(define-module (baba packages tailscale)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang))

(define-public go-software-sslmate-com-src-go-pkcs12
  (package
    (name "go-software-sslmate-com-src-go-pkcs12")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://software.sslmate.com/src/go-pkcs12.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r8hx5ply4c3jqkgaqlf4zsgg0rw54wbs2qlxf2m1skffb4gppj7"))))
    (build-system go-build-system)
    (arguments '(#:import-path "software.sslmate.com/src/go-pkcs12"))
    (propagated-inputs `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (home-page "https://github.com/SSLMate/go-pkcs12")
    (synopsis "package pkcs12")
    (description
     "Package pkcs12 implements some of PKCS#12 (also known as P12 or PFX).  It is
intended for decoding DER-encoded P12/PFX files for use with the crypto/tls
package, and for encoding P12/PFX files for use by legacy applications which do
not support newer formats.  Since PKCS#12 uses weak encryption primitives, it
SHOULD NOT be used for new applications.")
    (license license:bsd-3)))
