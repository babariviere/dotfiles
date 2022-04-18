(define-module (baba packages fonts)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system font)
  #:replace (font-iosevka
             font-iosevka-slab
             font-iosevka-term
             font-iosevka-term-slab
             font-iosevka-aile
             font-iosevka-curly
             font-iosevka-curly-slab
             font-iosevka-etoile))

#|
guix repl <<EOF
(use-modules (guix base32)
             (guix download)
             (guix packages)
             (guix store)
             (gcrypt hash)
             (ice-9 string-fun)
             (baba packages fonts))

(let ((new-version "15.1.0")
      (iosevka-hashes #nil)
      (iosevka-fails #nil))
  (for-each (lambda (font)
              (let ((file (download-to-store (open-connection)
                                             (string-replace-substring
                                              (origin-uri (package-source font))
                                              (package-version font)
                                              new-version))))
                (if file
                    (set! iosevka-hashes
                          (acons file (bytevector->nix-base32-string
                                       (file-sha256 file))
                                 iosevka-hashes))
                    (set! iosevka-fails (cons font iosevka-fails)))))
            (list font-iosevka
                  font-iosevka-slab
                  font-iosevka-term
                  font-iosevka-term-slab
                  font-iosevka-aile
                  font-iosevka-curly
                  font-iosevka-curly-slab
                  font-iosevka-etoile))
  (for-each (lambda (hash)
              (format #t "~a: ~a~%" (car hash) (cdr hash)))
            iosevka-hashes)
  (for-each (lambda (fail)
              (format #t "~a: failed to download latest version~%" fail))
            iosevka-fails))
EOF
|#
(define-public font-iosevka
  (package
    (name "font-iosevka")
    (version "15.1.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttf-iosevka-" version ".zip"))
       (sha256
        (base32 "1ds008lsqb818ahh20i5gd03rzfx2hzw2wlxc9bwj4l0sw43py6i"))))
    (build-system font-build-system)
    (home-page "https://be5invis.github.io/Iosevka/")
    (synopsis "Coders' typeface, built from code")
    (description
     "Iosevka is a slender monospace sans-serif or slab-serif typeface inspired
by Pragmata Pro, M+, and PF DIN Mono, designed to be the ideal font for
programming.  Iosevka is completely generated from its source code.")
    (license (list license:silofl1.1    ;build artifacts (i.e., the fonts)
                   license:bsd-3))))    ;supporting code

(define-public font-iosevka-slab
  (package
    (inherit font-iosevka)
    (name "font-iosevka-slab")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttf-iosevka-slab-" version ".zip"))
       (sha256
        (base32 "148m12yan3kl97ni3bw5ry8xxmzrwbz0my7ywszjxp3y2rr61n8a"))))))

(define-public font-iosevka-term
  (package
    (inherit font-iosevka)
    (name "font-iosevka-term")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttf-iosevka-term-" version ".zip"))
       (sha256
        (base32 "106hd8rcyjfa522qj6lscky9qzq2ikn02gmnxs72mvgm19janszg"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".*"))
             #t)))))))

(define-public font-iosevka-term-slab
  (package
    (inherit font-iosevka)
    (name "font-iosevka-term-slab")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka/"
                           "releases/download/v" version "/"
                           "ttf-iosevka-term-slab-" version ".zip"))
       (sha256
        (base32 "1vy931yrp82wxj1ggjbs2ahs95gvvzkmfb55v3rk2bshi6r0b3i1"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'make-files-writable
           (lambda _
             (for-each make-file-writable (find-files "." ".*"))
             #t)))))))

(define-public font-iosevka-aile
  (package
    (inherit font-iosevka)
    (name "font-iosevka-aile")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttf-iosevka-aile-" version ".zip"))
       (sha256
        (base32 "103jkm8plmqv8p9523w2i9lg5r6sdk5ip8ak37pvj90vh1hlf8mp"))))))

(define-public font-iosevka-curly
  (package
    (inherit font-iosevka)
    (name "font-iosevka-curly")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka/"
                           "releases/download/v" version  "/"
                           "ttf-iosevka-curly-" version ".zip"))
       (sha256
        (base32 "0b7pnpdc1wsxb6w7ff5l9kj78ijr45505qk6dnnipcr6gvvl4liv"))))))

(define-public font-iosevka-curly-slab
  (package
    (inherit font-iosevka)
    (name "font-iosevka-curly-slab")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka/"
                           "releases/download/v" version  "/"
                           "ttf-iosevka-curly-slab-" version ".zip"))
       (sha256
        (base32 "0nc5fl6lxbb5fd2snr6p4w9vl83yw8hhnjn4ir2nlvr8v5rawz08"))))))

(define-public font-iosevka-etoile
  (package
    (inherit font-iosevka)
    (name "font-iosevka-etoile")
    (version (package-version font-iosevka))
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://github.com/be5invis/Iosevka"
                           "/releases/download/v" version
                           "/ttf-iosevka-etoile-" version ".zip"))
       (sha256
        (base32 "1gnim2kng5gd88jc2miz5dj3pmfm1bqxnq2mgfc2wdf967dzyr8j"))))))



(define-public font-biosevka
  (package
   (name "font-biosevka")
   (version "15.2.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/babariviere/biosevka")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0s7jf2qxnzljlyyqdxgjgawigpkdyhhxlsibwl5gmi3q14szxds7"))))
   (build-system font-build-system)
   (arguments
    `(#:phases
      (modify-phases %standard-phases
                     (replace 'unpack
                              (lambda* (#:key source #:allow-other-keys)
                                (use-modules ((guix build gnu-build-system) #:prefix gnu:))
                                (define gnu:unpack (assoc-ref gnu:%standard-phases 'unpack))
                                (gnu:unpack #:source source)
                                (chdir "dist/biosevka/ttf"))))))
   (home-page "https://be5invis.github.io/Iosevka/")
   (synopsis "Coders' typeface, built from code")
   (description
    "Iosevka is a slender monospace sans-serif or slab-serif typeface inspired
by Pragmata Pro, M+, and PF DIN Mono, designed to be the ideal font for
programming.  Iosevka is completely generated from its source code.")
   (license (list license:silofl1.1 ;build artifacts (i.e., the fonts)
                  license:bsd-3))))
