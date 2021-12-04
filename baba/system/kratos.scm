(define-module (baba system kratos)
  #:use-module (baba)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages certs)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services cuirass)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (guix gexp)
  #:export (%system/kratos))

(define %cuirass-port 32800)

(define %cuirass-specs
  #~(list (specification
	   (name "emacs")
	   (build '(channels emacs))
	   (channels
	    (cons*
	     (channel
	      (name 'emacs)
	      (url "https://github.com/babariviere/guix-emacs"))
	     %default-channels)))
	  (specification
	   (name "flat")
	   (build '(channels flat))
	   (channels
	    (cons*
	     (channel
	      (name 'flat)
	      (url "https://github.com/flatwhatson/guix-channel.git"))
	     %default-channels)))))

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
   (packages (append (list
		      htop
		      ;; for HTTPS access
		      nss-certs
		      )
		     %base-packages))
   (services
    (append (list (service dhcp-client-service-type)
		  (service openssh-service-type
			   (openssh-configuration
			    (permit-root-login 'prohibit-password)
			    (allow-empty-passwords? #f)
			    ;; TODO: better system for ssh keys
			    (authorized-keys `(("root" ,(local-file (string-append %channel-root "/etc/ssh/gaia.pub")))))))
		  ;; (service guix-publish-service-type
		  ;; 	   (guix-publish-configuration
		  ;; 	    ))
		  (service cuirass-service-type
			   (cuirass-configuration
			    (specifications %cuirass-specs)
			    (port %cuirass-port)
			    (use-substitutes? #t))))
	    (modify-services
	     %base-services
	     (guix-service-type config =>
				(guix-configuration
				 (inherit config)
				 (discover? #f)
				 (authorized-keys (append
						   %default-authorized-guix-keys
						   (list (local-file (string-append %channel-root "/etc/keys/gaia.pub"))))))))))))

%system/kratos
