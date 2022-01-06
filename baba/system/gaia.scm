;; This is an operating system configuration template
;; for a "desktop" setup without full-blown desktop
;; environments.

(define-module (baba system gaia)
  #:use-module (srfi srfi-1)
  #:use-module (baba)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services desktop)
  #:use-module (gnu services nix)
  #:use-module (gnu services pm)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services xorg)
  #:use-module (gnu system nss)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (%system/gaia))

(define %blacklist-modules
  (list "pcspkr" "snd_pcsp"))

(define xorg.conf
  "
Section \"ServerLayout\"
  Identifier \"layout\"
  Screen 0 \"nouveau\"
  Inactive \"iGPU\"
EndSection

Section \"Device\"
  Identifier \"iGPU\"
  Driver \"modesetting\"
  Option \"DRI\" \"3\"
  BusID \"PCI:0:2:0\"
EndSection

Section \"Screen\"
  Identifier \"intel\"
  Device \"iGPU\"
EndSection

Section \"Device\"
  Identifier \"dGPU\"
  Driver \"nouveau\"
  BusID \"PCI:1:0:0\"
EndSection

Section \"Screen\"
  Identifier \"nouveau\"
  Device \"dGPU\"
EndSection
")

(define services
  (cons*
   (service nix-service-type)
   (service tlp-service-type)
   (service slim-service-type (slim-configuration
                               (display ":0")
                               (vt "vt7")
			       (xorg-configuration
				(xorg-configuration
				 (extra-config (list xorg.conf))))))
   (modify-services
    (remove (lambda (service)
              (eq? (service-kind service) gdm-service-type))
            %desktop-services)
    (guix-service-type config =>
		       (guix-configuration
			(inherit config)
			(discover? #t)
			(substitute-urls (append
					  (@@ (guix scripts substitute) %default-substitute-urls)
					  (list "https://substitutes.nonguix.org" "https://ci.babariviere.com")))
			(authorized-keys (append
					  %default-authorized-guix-keys
					  (list
					   (local-file (string-append %channel-root "/etc/keys/substitutes.nonguix.org.pub"))
					   (local-file (string-append %channel-root "/etc/keys/ci.babariviere.com.pub"))))))))))

(define %system/gaia
  (operating-system
   (host-name "gaia")
   (timezone "Europe/Paris")
   (locale "en_US.utf8")

   ;; Use the UEFI variant of GRUB with the EFI System
   ;; Partition mounted on /boot/efi.
   (bootloader (bootloader-configuration
		(bootloader grub-efi-bootloader)
		(targets '("/boot/efi"))))

   (kernel linux)
   (kernel-loadable-modules (list acpi-call-linux-module))
   (kernel-arguments
    (cons* (string-append "modprobe.blacklist="
			  (string-join %blacklist-modules
				       ","))
	   (delete "quiet" %default-kernel-arguments)))
   (keyboard-layout (keyboard-layout
		     "us" "altgr-intl"
		     #:options '("ctrl:nocaps")))
   (firmware (list linux-firmware))
   (initrd microcode-initrd)
   ;; Assume the target root file system is labelled "my-root",
   ;; and the EFI System Partition has UUID 1234-ABCD.
   (file-systems (append
		  (list (file-system
			 (device (uuid "d02d6b18-5f6a-4150-8669-aea28343e0b4"))
			 (mount-point "/")
			 (type "ext4"))
			(file-system
			 (device (uuid "E523-A561" 'fat))
			 (mount-point "/boot/efi")
			 (type "vfat")))
		  %base-file-systems))

   (users (cons (user-account
		 (name "babariviere")
		 (group "users")
		 (supplementary-groups '("wheel" "netdev"
					 "audio" "video"
					 "kvm"))
		 (shell (file-append fish "/bin/fish")))
		%base-user-accounts))

   ;; Add a bunch of window managers; we can choose one at
   ;; the log-in screen with F1.
   (packages (append (list
		      ;; window managers
		      sway dmenu

		      ;; TODO: stumpwm service
		      stumpwm
		      `(,stumpwm "lib")
		      sbcl-stumpwm-ttf-fonts
		      sbcl
		      ;; emacs
		      ;; terminal emulator
		      alacritty foot xterm neovim
		      ;; ssh
		      openssh
		      ;; for HTTPS access
		      nss-certs
		      ;; for hetzner kvm console
		      icedtea icedtea-web
		      ;; tools
		      gnu-make

		      ;; x11
		      xrandr
		      autorandr
		      picom)
		     %base-packages))

   ;; Use the "desktop" services, which include the X11
   ;; log-in service, networking with NetworkManager, and more.
   (services services)

   ;; Allow resolution of '.local' host names with mDNS.
   (name-service-switch %mdns-host-lookup-nss)))

%system/gaia
