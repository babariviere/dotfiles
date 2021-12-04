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
  #:use-module (guix gexp)
  #:export (%system/kratos))

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
			 (device (file-system-label "root"))
			 (type "btrfs")
			 (options "subvol=system,compress=zstd"))
			%base-file-systems))
   (services
    (append (list (service dhcp-client-service-type)
		  (service openssh-service-type
			   (openssh-configuration
			    (permit-root-login 'prohibit-password)
			    (allow-empty-passwords? #f)
			    ;; TODO: better system for ssh keys
			    (authorized-keys `(("root" ,(local-file (string-append %channel-root "/etc/ssh/gaia.pub"))))))))
	    (modify-services
	     %base-services
	     (guix-service-type config =>
				(guix-configuration
				 (inherit config)
				 (discover? #t)
				 (authorized-keys (append
						   %default-authorized-guix-keys
						   (list (local-file (string-append %channel-root "/etc/keys/gaia.pub"))))))))))))

%system/kratos
