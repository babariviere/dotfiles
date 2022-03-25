(define-module (baba packages file-systems)
  #:use-module (gnu packages)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu system file-systems)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

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
(module-define! (resolve-module '(gnu system linux-initrd)) 'file-system-packages
                (lambda* (file-systems #:key (volatile-root? #f))
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
                          '()))))
