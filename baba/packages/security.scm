(define-module (baba packages security)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ssh)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public keychain
  (package
    (name "keychain")
    (version "2.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/funtoo/keychain")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bkjlg0a2bbdjhwp37ci1rwikvrl4s3xlbf2jq2z4azc96dr83mj"))))
    (build-system trivial-build-system)
    (inputs
     (list bash coreutils findutils gawk gnupg grep sed openssh procps))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       ,#~(begin
            (use-modules (guix build utils))
            (let ((source #$source)
                  (out #$output))
              (copy-recursively source ".")
              (substitute* "keychain"
                (("/bin/sh") (string-append #$(this-package-input "bash") "/bin/bash")))
              (mkdir-p (string-append out "/share/man/man1"))
              (install-file "keychain.1" (string-append out "/share/man/man1"))
              (install-file "keychain" (string-append out "/bin"))
              (wrap-program (string-append out "/bin/keychain")
                #:sh (string-append #$(this-package-input "bash") "/bin/bash")
                `("PATH" ":" prefix (,#$@(map (lambda (v) (file-append (second v) "/bin"))
                                              (package-inputs this-package)))))))))
    (home-page "https://github.com/funtoo/keychain")
    (synopsis "Keychain helps you to manage ssh and GPG keys in a convenient and secure manner.")
    (description "Keychain helps you to manage ssh and GPG keys in a convenient and secure manner.
It acts as a frontend to ssh-agent and ssh-add, but allows you to easily have one long running ssh-agent
 process per system, rather than the norm of one ssh-agent per login session.")
    (license license:gpl2)))
