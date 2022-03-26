(define-module (baba packages linux)
  #:use-module (gnu packages linux)
  #:use-module (nongnu packages linux)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public linux-bcachefs
  (package
   (inherit ((@@ (gnu packages linux) make-linux-libre*)
             linux-libre-version
             linux-libre-gnu-revision
             (origin
              (inherit linux-libre-5.16-source)
              (method git-fetch)
              (uri (git-reference
                    (url "https://evilpiepirate.org/git/bcachefs.git")
                    (commit "c38b7167aa5f3b1b91dcc93ade57f30e95064590")))
              (sha256
               (base32 "1xsrisjh8xjippm3higvwidisq7sdydxxzm7pyr06xgpqwffcfg3"))
              (file-name (git-file-name "linux-bcachefs" linux-libre-version)))
             '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux")
             #:configuration-file (@@ (gnu packages linux) kernel-config)
             #:extra-options (acons "CONFIG_BCACHEFS_FS" 'm (@@ (gnu packages linux) %default-extra-linux-options))))
   (name "linux-testing")))
