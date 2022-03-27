(define-module (baba bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (grub-efi-inplace-bootloader))

(define (linux-path kernel mount-point)
  #~(begin
      (string-append #$mount-point "linux/" (basename (dirname #$kernel)))))

(define (initrd-path initrd mount-point)
  #~(begin
      (string-append #$mount-point "initrd/" (basename (dirname #$initrd)))))

(define* (inplace-generator config entries
                            #:key
                            (locale #f)
                            (system (%current-system))
                            (old-entries '())
                            (store-crypto-devices '())
                            store-directory-prefix)
  (define (patch-menu-entry entry)
    (menu-entry
     (inherit entry)
     (linux (linux-path (menu-entry-linux entry) (menu-entry-device-mount-point entry)))
     (initrd (initrd-path (menu-entry-initrd entry) (menu-entry-device-mount-point entry)))))
  ((@@ (gnu bootloader grub) grub-configuration-file)
   config
   (map patch-menu-entry entries)
   #:locale locale
   #:system system
   #:old-entries (map patch-menu-entry old-entries)
   #:store-crypto-devices store-crypto-devices
   #:store-directory-prefix store-directory-prefix))

(define install-boot-kernel
  #~(lambda (kernel mount-point)
      (let* ((target (string-append mount-point "/boot/linux/" (basename (dirname kernel))))
             (pivot (string-append target ".new")))
        (mkdir-p (dirname target))

        (copy-file kernel pivot)
        (rename-file pivot target))))

(define install-boot-initrd
  #~(lambda (initrd mount-point)
      (let* ((target (string-append mount-point "/boot/initrd/" (basename (dirname initrd))))
             (pivot (string-append target ".new")))
        (mkdir-p (dirname target))

        (copy-file initrd pivot)
        (rename-file pivot target))))

(define inplace-installer
  #~(lambda (bootloader efi-dir mount-point)
      (use-modules (ice-9 regex) (ice-9 textual-ports) (srfi srfi-1))

      (define (recursive-readlink l)
        (format #t "~A~%" (stat:type (lstat l)))
        (if (eq? (stat:type (lstat l)) 'symlink)
            (recursive-readlink (readlink l))
            l))
      (when efi-dir
        (let ((dir (opendir "/var/guix/profiles"))
              (kernels '())
              (initrds '()))
          (do ((entry (readdir dir) (readdir dir)))
              ((eof-object? entry))
            (when (and (string-contains entry "system") (string-contains entry "link"))
              (let ((initrd (recursive-readlink (string-append "/var/guix/profiles/" entry "/initrd")))
                    (kernel (recursive-readlink (string-append "/var/guix/profiles/" entry "/kernel/bzImage"))))
                (format #t "~a~%" kernel)
                (set! kernels (cons kernel kernels))
                (set! initrds (cons initrd initrds)))))
          (closedir dir)

          (map (lambda (image) (#$install-boot-kernel image mount-point)) (delete-duplicates kernels))
          (map (lambda (image) (#$install-boot-initrd image mount-point)) (delete-duplicates initrds)))
        (#$(@@ (gnu bootloader grub) install-grub-efi) bootloader efi-dir mount-point))))

(define grub-efi-inplace-bootloader
  (bootloader
   (inherit grub-efi-bootloader)
   (name 'grub-efi-inplace-bootloader)
   (configuration-file-generator inplace-generator)
   (installer inplace-installer)))
