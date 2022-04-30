(define-module (baba home gaia)
  #:use-module (baba)
  #:use-module (baba home services browsers)
  #:use-module (baba home services emacs)
  #:use-module (baba home services fonts)
  #:use-module (baba home services terminals)
  #:use-module (baba home services wm)
  #:use-module (baba packages fonts)
  #:use-module (baba packages security)
  #:use-module (brycus home-service)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home-services emacs)
  #:use-module (baba home services gnupg)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (flat packages emacs)
  #:use-module (nongnu packages mozilla)
  #:use-module (srfi srfi-1))

(define %sway-config
  `( ;; (bindsym $mod+Shift+e exec emacsclient -c --eval "(eshell)")
    (xwayland enable)
    (set $term ,(file-append foot "/bin/foot"))
    (set $menu ,(file-append rofi "/bin/rofi -modi drun -show drun"))

    (set $mod Mod4)
    (set $left h)
    (set $down j)
    (set $up k)
    (set $right l)

    (bindsym $mod+Return exec $term)
    (bindsym $mod+Shift+q kill)
    (bindsym $mod+d exec $menu)

    (floating_modifier $mod normal)

    (bindsym $mod+Shift+c reload)

    (output * bg ,(file-append sway "/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png") fill)

    (bindsym $mod+$left focus left)
    (bindsym $mod+$down focus down)
    (bindsym $mod+$up focus up)
    (bindsym $mod+$right focus right)

    ;; Move the focused window with the same, but add Shift
    (bindsym $mod+Shift+$left move left)
    (bindsym $mod+Shift+$down move down)
    (bindsym $mod+Shift+$up move up)
    (bindsym $mod+Shift+$right move right)

    ;; Workspaces:

    ;; Switch to workspace
    (bindsym $mod+1 workspace number 1)
    (bindsym $mod+2 workspace number 2)
    (bindsym $mod+3 workspace number 3)
    (bindsym $mod+4 workspace number 4)
    (bindsym $mod+5 workspace number 5)
    (bindsym $mod+6 workspace number 6)
    (bindsym $mod+7 workspace number 7)
    (bindsym $mod+8 workspace number 8)
    (bindsym $mod+9 workspace number 9)
    (bindsym $mod+0 workspace number 10)
    ;; Move focused container to workspace
    (bindsym $mod+Shift+1 move container to workspace number 1)
    (bindsym $mod+Shift+2 move container to workspace number 2)
    (bindsym $mod+Shift+3 move container to workspace number 3)
    (bindsym $mod+Shift+4 move container to workspace number 4)
    (bindsym $mod+Shift+5 move container to workspace number 5)
    (bindsym $mod+Shift+6 move container to workspace number 6)
    (bindsym $mod+Shift+7 move container to workspace number 7)
    (bindsym $mod+Shift+8 move container to workspace number 8)
    (bindsym $mod+Shift+9 move container to workspace number 9)
    (bindsym $mod+Shift+0 move container to workspace number 10)

    ;; Layout stuff:
    (bindsym $mod+b splith)
    (bindsym $mod+v splitv)

    (bindsym $mod+f fullscreen)

    (bindsym $mod+Shift+space floating toggle)
    (bindsym $mod+space focus mode_toggle)

    ;; Scratchpad:
    (bindsym $mod+Shift+minus move scratchpad)
    (bindsym $mod+minus move scratchpad)

    ;; Bar:
    (bar ((position top)
      (colors ((statusline "#ffffff")
           (background "#323232")))
      (status_command "while date +'%Y-%m-%d %l:%M:%S %p'; do sleep 1; done")))

    ;; Input:
    (input * ((xkb_layout "us")
          (xkb_variant "altgr-intl")
          (xkb_options "ctrl:nocaps")))
    ;; Output:
    (output HDMI-A-1 res 2560x1440@60Hz pos 0 0)
    (output DP-2 res 2560x1440@60Hz pos 2560 0)
    (output eDP-1 pos ,(inexact->exact (/ 3840 2)) 1440)
    (bindswitch lid:on output eDP-1 disable)
    (bindswitch lid:off output eDP-1 enable)
    (focus_follows_mouse no)))

(define gmail-folder-mapping
  '(("inbox"   . "INBOX")
    ("sent"    . "[Gmail]/Sent Mail")
    ("drafts"  . "[Gmail]/Drafts")
    ("archive" . "[Gmail]/All Mail")
    ("trash"   . "[Gmail]/Trash")
    ("spam"    . "[Gmail]/Spam")))

(define fastmail-folder-mapping
  '(("inbox" . "INBOX")
    ("sent" . "Sent")
    ("drafts" . "Drafts")
    ("archive" . "Archive")
    ("trash" . "Trash")
    ("spam" . "Spam")))

(define (prep-str sym str)
  (symbol-append sym '- (string->symbol str)))

(define (isync-channel id local remote)
  `((Channel ,(prep-str id local))
    (Near ,(format #f ":~a-local:~a" id local))
    (Far ,(format #f ":~a-remote:~a" id remote))
    ,#~""))

(define (isync-group-with-channels id isync-mapping)
  (append
   (append-map
    (lambda (x) (isync-channel id (car x) (cdr x)))
    isync-mapping)
   `((Group ,(symbol-append id))
     ,@(map
        (lambda (x) (list 'Channel (prep-str id (car x))))
        isync-mapping)
     ,#~"")))

(define (gpg-cmd id host field)
  (string-append "gpg -q --for-your-eyes-only --no-tty -d ~/.authinfo.gpg | awk '/machine " host " id " (symbol->string id) "/ {print " field "}'"))

;; Expected authinfo format:
;; machine <host> id <id> login <mail> port 993 password <pwd>
(define (mail-account id host folders-mapping)
  (let ((user-cmd (gpg-cmd id host "$(NF-4)"))
        (pass-cmd (gpg-cmd id host "$NF")))
    `((IMAPAccount ,id)
      (Host ,host)
      (UserCmd ,user-cmd)
      (PassCmd ,pass-cmd)
      (SSLType IMAPS)
      ,#~""
      (IMAPStore ,(symbol-append id '-remote))
      (Account ,id)
      ,#~""
      (MaildirStore ,(symbol-append id '-local))
      (Subfolders Verbatim)
      (Path ,(string-append "~/.mail/" (symbol->string id) "/"))
      (Inbox ,(string-append "~/.mail/" (symbol->string id) "/inbox"))
      ,#~""
      ,@(isync-group-with-channels id folders-mapping))))

(define default-isync-global-settings
  `((Create Both)
    (Expunge Both)
    (SyncState *)
    ,#~""))

;; TODO: make service for mbsync and notmuch
(home-environment
 (packages
  (list htop notmuch bat direnv keychain gnupg firefox flatpak))
 (services
  (append emacs-service
          nyxt-service
          stumpwm-service
          (list (service home-bash-service-type
                         (home-bash-configuration
                          (guix-defaults? #t)))
                (service home-fish-service-type
                         (home-fish-configuration
                          (config
                           (list
                            (local-file (etc-file "/fish/config.fish"))))
                          (abbreviations '(("gco" . "git checkout")
                                           ("gs" . "git status")
                                           ("gsr" . "sudo -E guix system reconfigure")
                                           ("ghr" . "guix home reconfigure")
                                           ("cat" . "bat -pp")))))
                (service home-brycus-fish-service-type)
                (service home-gnupg-service-type
                         (home-gnupg-configuration
                          (gpg-config
                           (home-gpg-configuration
                            (extra-config
                             ;; Use hardened config from https://github.com/drduh/YubiKey-Guide#harden-configuration=
                             '((personal-cipher-preferences . ("AES256" "AES192" "AES"))
                               (personal-digest-preferences . ("SHA512" "SHA384" "SHA256"))
                               (personal-compress-preferences . ("ZLIB" "BZIP2" "ZIP" "Uncompressed"))
                               (default-preference-list . ("SHA512" "SHA384" "SHA256" "AES256" "AES192" "AES" "ZLIB" "BZIP2" "ZIP" "Uncompressed"))
                               (cert-digest-algo . "SHA512")
                               (s2k-digest-algo . "SHA512")
                               (s2k-cipher-algo . "AES256")
                               (charset . "utf-8")
                               (fixed-list-mode)
                               (no-comments)
                               (no-emit-version)
                               (keyid-format . "0xlong")
                               (list-options . "show-uid-validity")
                               (verify-options . "show-uid-validity")
                               (with-fingerprint)
                               (require-cross-certification)
                               (no-symkey-cache)
                               (use-agent)))))
                          (gpg-agent-config
                           (home-gpg-agent-configuration
                            (ssh-agent? #t)))))
                (service home-git-service-type
                         (home-git-configuration
                          (config
                           `((user
                              ((name . "Bastien Riviere")
                               (email . "me@babariviere.com")
                               (signingKey . "39035CC0B75D1142")))
                             (github
                              ((user . "babariviere")))
                             (remote
                              ((pushDefault . "origin")))
                             (commit
                              ((gpgSign . #t)))
                             (tag
                              ((gpgSign . #t)))))))
                (service home-sway-service-type
                         (home-sway-configuration
                          (config %sway-config)))
                (service home-foot-service-type
                         (home-foot-configuration
                          (config
                           `((main
                              ((term . "xterm-256color")
                               (font . "MonoLisa:size=10")
                               (dpi-aware . "no")
                               (include . ,(file-append (package-source foot) "/themes/dracula"))))))))
                (service home-font-service-type
                         (home-font-configuration
                          (sans-serif (make-font-spec font-abattis-cantarell "Cantarell"))
                          (serif (make-font-spec font-liberation "Times New Roman"))
                          (monospace (make-font-spec font-biosevka "Biosevka"))))
                (simple-service 'setup-flatpak
                                home-environment-variables-service-type
                                `(("XDG_DATA_DIRS" . "$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS")
                                  ("PATH" . "$HOME/.local/share/flatpak/exports/bin:$PATH")))
                (service home-isync-service-type
                         (home-isync-configuration
                          (config
                           (append default-isync-global-settings
                                   (mail-account 'prv-fm
                                                 "imap.fastmail.com"
                                                 fastmail-folder-mapping)))))
                (simple-service 'isync-ensure-mail-dirs
                                home-activation-service-type
                                #~(map mkdir-p '#$(map (lambda (id) (string-append "~/.mail/" (symbol->string id))) '(prv-fm))))))))
