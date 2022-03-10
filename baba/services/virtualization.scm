(define-module (baba services virtualization)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:export (lxd-configuration lxd-service-type))

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join
     (string-split (string-delete #\? str) #\-)
     "_")))

(define (serialize-field field-name val)
  (format #t "~a = ~a\n" (uglify-field-name field-name) val))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val 1 0)))

;;;
;;; LXD linux container daemon.
;;;

(define-configuration lxd-configuration
  (lxd
   (package lxd)
   "LXD package.")
  (debug?
   (boolean #f)
   "Enable or disable debug messages.")
  (verbose?
   (boolean #f)
   "Enable or disable information messages."))

(define %lxd-accounts
  (list (user-group (name "lxd") (system? #t))))

(define (%lxd-activation config)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/log/lxd")))

(define (lxd-shepherd-service config)
  (let* ((lxd (lxd-configuration-lxd config))
         (debug? (lxd-configuration-debug? config))
         (verbose? (lxd-configuration-verbose? config)))
    (list
     (shepherd-service
      (documentation "LXD daemon.")
      (provision '(lxd))
      (requirement '(dbus-system
                     elogind
                     file-system-/sys/fs/cgroup/blkio
                     file-system-/sys/fs/cgroup/cpu
                     file-system-/sys/fs/cgroup/cpuset
                     file-system-/sys/fs/cgroup/devices
                     file-system-/sys/fs/cgroup/memory
                     file-system-/sys/fs/cgroup/pids
                     file-system-/sys/fs/cgroup/systemd
                     networking
                     udev))
      (start #~(make-forkexec-constructor
                (list (string-append #$lxd "/bin/lxd")
                      "--group=lxd"
                      "--logfile=/var/log/lxd/lxd.log"
                      #$@(if debug? '("--debug") '())
                      #$@(if verbose? '("--verbose") '()))))
      (stop #~(make-kill-destructor))))))

(define %lxd-cgroup
  (file-system
    (device "cgroup")
    (mount-point "/sys/fs/cgroup/systemd")
    (type "cgroup")
    (check? #f)
    (options "none,name=systemd")
    (create-mount-point? #t)
    (dependencies (list (car %control-groups)))))

(define lxd-service-type
  (service-type
   (name 'lxd)
   (extensions
    (list (service-extension activation-service-type
                             %lxd-activation)
          (service-extension profile-service-type
                             (lambda (config)
                               (list
                                (lxd-configuration-lxd config))))
          (service-extension shepherd-root-service-type
                             lxd-shepherd-service)
          (service-extension account-service-type
                             (const %lxd-accounts))
          (service-extension file-system-service-type
                             (const (list %lxd-cgroup)))))
   (default-value (lxd-configuration))))
