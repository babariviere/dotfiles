(define-module (baba packages file-systems)
  #:use-module (gnu packages)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages linux)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system linux-initrd)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (bcachefs-initrd))

(define bcachefs-commit "f3cdace86c8b60a4efaced23b2d31c16dc610da9")
(define bcachefs-version (git-version "0.1" "15" bcachefs-commit))
(define bcachefs-source
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://evilpiepirate.org/git/bcachefs-tools.git")
         (commit bcachefs-commit)))
   (sha256
    (base32 "1hg4cjrs4yr0mx3mmm1jls93w1skpq5wzp2dzx9rq4w5il2xmx19"))
   (file-name (git-file-name "bcachefs-tools" bcachefs-version))))

(define-public bcachefs-tools-git
  (package
   (inherit bcachefs-tools)
   (version bcachefs-version)
   (source bcachefs-source)))

(define-public bcachefs-tools-git/static
  (package
   (inherit bcachefs-tools/static)
   (version bcachefs-version)
   (source bcachefs-source)))

(define-public bcachefs-git/static
  (package
   (inherit bcachefs/static)
   (inputs (list bcachefs-tools-git/static))))

;; Try to redefine `file-system-packages` to use up-to-date bcachefs tools
(define* (file-system-packages* file-systems #:key (volatile-root? #f))
  `(,@(if (find (lambda (fs)
                  (string-prefix? "ext" (file-system-type fs)))
                file-systems)
          (list e2fsck/static)
          '())
    ,@(if (find (lambda (fs)
                  (string-suffix? "fat" (file-system-type fs)))
                file-systems)
          (list fatfsck/static)
          '())
    ,@(if (find (file-system-type-predicate "bcachefs") file-systems)
          (list bcachefs-git/static)
          '())
    ,@(if (find (file-system-type-predicate "btrfs") file-systems)
          (list btrfs-progs/static)
          '())
    ,@(if (find (file-system-type-predicate "jfs") file-systems)
          (list jfs_fsck/static)
          '())
    ,@(if (find (file-system-type-predicate "ntfs") file-systems)
          (list ntfsfix/static)
          '())
    ,@(if (find (file-system-type-predicate "f2fs") file-systems)
          (list f2fs-fsck/static)
          '())
    ,@(if (find (file-system-type-predicate "xfs") file-systems)
          (list xfs_repair/static)
          '())))

;; FIXME: unable to find initrd.cpio.gz when using this initrd (there is minimal diff with base-initrd)
(define* (bcachefs-initrd file-systems
                          #:key
                          (linux linux-libre)
                          (linux-modules '())
                          (mapped-devices '())
                          (keyboard-layout #f)
                          qemu-networking?
                          volatile-root?
                          (extra-modules '()) ;deprecated
                          (on-error 'debug))
  (define linux-modules*
    ;; Modules added to the initrd and loaded from the initrd.
    `(,@linux-modules
      ,@((@@ (gnu system linux-initrd) file-system-modules) file-systems)
      ,@(if volatile-root?
            '("overlay")
            '())
      ,@extra-modules))

  (define helper-packages
    (append (file-system-packages* file-systems
                                   #:volatile-root? volatile-root?)
            (if keyboard-layout
                (list loadkeys-static)
                '())))

  (format #t "~a~%" linux-modules*)

  (raw-initrd file-systems
              #:linux linux
              #:linux-modules linux-modules*
              #:mapped-devices mapped-devices
              #:helper-packages helper-packages
              #:keyboard-layout keyboard-layout
              #:qemu-networking? qemu-networking?
              #:volatile-root? volatile-root?
              #:on-error on-error))
