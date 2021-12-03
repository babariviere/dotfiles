(define-module (baba system kratos)
  #:use-module (baba)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (guix gexp))

(define %system/kratos
  (operating-system
   (host-name "kratos")
   (timezone "Europe/Paris")
   (bootloader (bootloader-configuration
		(bootloader grub-bootloader)
		(targets '("/dev/sda"))
		(terminal-outputs '(console))))
   (file-systems (cons* (file-system
			 (mount-point "/")
			 (device (file-system-label "tank"))
			 (type "btrfs")
			 (options "subvol=system,compress=zstd"))
			(file-system
			 (mount-point "/boot")
			 (device "/dev/sda1")
			 (type "ext4")
			 (needed-for-boot? #t))
			(file-system
			 (mount-point "/gnu/store")
			 (device (file-system-label "tank"))
			 (type "btrfs")
			 (options "subvol=system/gnu/store,compress-force=zstd,space_cache=v2"))
			%base-file-systems))
   (services
    (append (list (service dhcp-client-service-type)
		  (service openssh-service-type
			   (openssh-configuration
			    (permit-root-login 'prohibit-password)
			    (allow-empty-passwords? #f)
			    ;; TODO: better system for ssh keys
			    (authorized-keys `(("root" ,(local-file (string-append %channel-root "/etc/ssh/gaia.pub"))))))))
	    %base-services))))

%system/kratos
