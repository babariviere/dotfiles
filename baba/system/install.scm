(define-module (baba system install)
  #:use-module (baba packages file-systems)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

(define installation-os-nonfree
  (operating-system
   (inherit installation-os)
   (kernel linux)
   (firmware (list linux-firmware))
   (packages (cons* bcachefs-tools-git (operating-system-packages installation-os)))))

installation-os-nonfree
