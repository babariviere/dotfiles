;; This is an operating system configuration template
;; for a "desktop" setup without full-blown desktop
;; environments.

(define-module (baba system gaia)
  #:use-module (srfi srfi-1)
  #:use-module (baba)
  #:use-module (baba bootloader grub)
  #:use-module (baba packages linux)
  #:use-module (baba packages file-systems)
  #:use-module (baba services virtualization)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services cups)
  #:use-module (gnu services desktop)
  #:use-module (gnu services nix)
  #:use-module (gnu services pm)
  #:use-module (gnu services security-token)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services xorg)
  #:use-module (gnu system nss)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd)
  #:export (%system/gaia))

(define %blacklist-modules
  (list "pcspkr" "snd_pcsp"))

(define xorg.conf
  "
Section \"Device\"
  Identifier \"dGPU\"
  Driver \"modesetting\"
  BusID \"PCI:1:0:0\"
EndSection

Section \"Screen\"
  Identifier \"nouveau\"
  Device \"dGPU\"
EndSection
")

(define libinput.conf
  "
Section \"InputClass\"
    Identifier \"touchpad\"
    Driver \"libinput\"
    MatchDevicePath \"/dev/input/event*\"
    MatchIsTouchpad \"on\"

    Option \"Tapping\" \"off\"
    Option \"ClickMethod\" \"clickfinger\"
    Option \"DisableWhileTyping\" \"on\"
    Option \"ScrollMethod\" \"twofinger\"
EndSection

Section \"InputClass\"
    Identifier \"keyboard\"
    Driver \"libinput\"
    MatchDevicePath \"/dev/input/event*\"
    MatchIsKeyboard \"on\"
EndSection
")

(define %keyboard-layout
  (keyboard-layout
   "us" "altgr-intl"))

(define services
  (cons*
   (service nix-service-type)
   (service tlp-service-type
            (tlp-configuration
             (cpu-scaling-max-freq-on-ac 4000000)
             (cpu-scaling-max-freq-on-bat 4000000)
             (start-charge-thresh-bat0 75)
             (stop-charge-thresh-bat0 80)))
   (service slim-service-type (slim-configuration
                               (display ":0")
                               (vt "vt7")
                               (xorg-configuration
                                (xorg-configuration
                                 (extra-config (list xorg.conf libinput.conf))))))
   (service cups-service-type
            (cups-configuration
             (web-interface? #t)
             (extensions
              (list cups-filters epson-inkjet-printer-escpr hplip-minimal))))
   (service lxd-service-type)
   (service pcscd-service-type)
   (udev-rules-service 'yubikey yubikey-personalization)
   (bluetooth-service #:auto-enable? #t)
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
                                          (list "https://substitutes.nonguix.org")))
                        (authorized-keys (append
                                          %default-authorized-guix-keys
                                          (list
                                           (local-file (string-append %channel-root "/etc/keys/substitutes.nonguix.org.pub"))))))))))

(define* (gaia/initrd file-systems
                      #:key
                      (linux-modules '())
                      (extra-modules '())
                      #:allow-other-keys
                      #:rest rest)
  (define linux-modules*
    ;; Modules added to the initrd and loaded from the initrd.
    `(,@linux-modules
      "bcachefs"
      ,@extra-modules))

  (define helper-packages
    (list bcachefs-git/static e2fsck/static loadkeys-static))

  (apply raw-initrd file-systems
         #:linux-modules linux-modules*
         #:helper-packages helper-packages
         (strip-keyword-arguments '(#:linux-modules #:extra-modules #:helper-packages) rest)))

(define %system/gaia
  (operating-system
    (host-name "gaia")
    (timezone "Europe/Paris")
    (locale "en_US.utf8")

    ;; Use the UEFI variant of GRUB with the EFI System
    ;; Partition mounted on /boot/efi.
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-inplace-bootloader)
                 (targets '("/boot/efi"))))

    (kernel linux-bcachefs)
    (kernel-loadable-modules (list acpi-call-linux-module))
    (kernel-arguments
     (cons* (string-append "modprobe.blacklist="
                           (string-join %blacklist-modules
                                        ","))
            (delete "quiet" %default-kernel-arguments)))
    (keyboard-layout %keyboard-layout)
    (firmware (list linux-firmware))
    (initrd (lambda (file-systems . rest)
              (apply microcode-initrd file-systems
                     #:initrd gaia/initrd
                     #:microcode-packages (list intel-microcode)
                     rest)))
    ;; Assume the target root file system is labelled "my-root",
    ;; and the EFI System Partition has UUID 1234-ABCD.
    (file-systems (append
                   (list (file-system
                          (mount-point "/")
                          (device "/dev/nvme1n1p3")
                          (check? #f) ; FIXME: enable check when we have found a solution to override bcachefs version
                          (needed-for-boot? #t)
                          (type "bcachefs"))
                         (file-system
                          (mount-point "/boot")
                          (device (uuid "4abb9807-c961-405f-81a2-1d9bd84f1360" 'ext4))
                          (needed-for-boot? #t)
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
                                          "kvm" "lxd"
                                          "lp" "lpadmin"))
                  (shell (file-append fish "/bin/fish")))
                 %base-user-accounts))

    ;; Add a bunch of window managers; we can choose one at
    ;; the log-in screen with F1.
    (packages (append (list
                       ;; window managers
                       sway dmenu

                       ;; terminal emulator
                       alacritty foot xterm neovim
                       ;; ssh
                       openssh
                       ;; for HTTPS access
                       nss-certs
                       ;; tools
                       gnu-make

                       ;; x11
                       xrandr
                       autorandr

                       ;; bluetooth
                       bluez

                       ;; file system
                       bcachefs-tools-git
                       )
                      %base-packages))

    ;; Use the "desktop" services, which include the X11
    ;; log-in service, networking with NetworkManager, and more.
    (services services)

    ;; Allow resolution of '.local' host names with mDNS.
    (name-service-switch %mdns-host-lookup-nss)))

%system/gaia
