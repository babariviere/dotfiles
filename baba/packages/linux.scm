(define-module (baba packages linux)
  #:use-module (gnu packages linux)
  #:use-module (nongnu packages linux)
  #:use-module (guix git-download)
  #:use-module (guix packages))

#|
guix repl -L . <<EOF
(use-modules (guix base32)
             (guix git)
             (guix git-download)
             (guix hash)
             (guix packages)
             (guix store)
             (baba packages linux))

(let* ((source (package-source linux-bcachefs))
       (url (git-reference-url (origin-uri source))))
  (call-with-values (lambda () (update-cached-checkout url))
    (lambda (path commit starting-commit?)
      (let ((hash (file-hash* path)))
        (format #t "commit: ~A~%hash: ~A~%" commit (bytevector->nix-base32-string hash))))))
EOF
|#

(define-public linux-bcachefs
  (package
    (inherit ((@@ (gnu packages linux) make-linux-libre*)
              linux-libre-version
              linux-libre-gnu-revision
              (origin
                (inherit linux-libre-5.17-source)
                (method git-fetch)
                (uri (git-reference
                      (url "https://evilpiepirate.org/git/bcachefs.git")
                      (commit "ca639d9221b8eb7c94e5a0bc118d53d8b453c0cf")))
                (sha256
                 (base32 "03krw388lak7yrbcv541vpind9v6v3x60ky0vfrss62dxi8nc2i5"))
                (file-name (git-file-name "linux-bcachefs" linux-libre-version)))
              '("x86_64-linux" "i686-linux" "armhf-linux" "aarch64-linux")
              #:configuration-file (@@ (gnu packages linux) kernel-config)
              #:extra-options (acons "CONFIG_BCACHEFS_FS" 'm (@@ (gnu packages linux) %default-extra-linux-options))))
    (name "linux-testing")))
