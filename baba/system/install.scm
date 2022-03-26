(define-module (baba system install)
  #:use-module (baba packages linux)
  #:use-module (baba packages file-systems)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

(define replace-bcachefs
  (package-input-rewriting/spec
   `(("bcachefs-tools" . ,(const bcachefs-tools-git))
     ("bcachefs" . ,(const bcachefs-git/static)))))

(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel linux-bcachefs)
    (firmware (list linux-firmware))
    (packages (cons* bcachefs-tools-git (map replace-bcachefs (operating-system-packages installation-os))))))

installation-os-nonfree
