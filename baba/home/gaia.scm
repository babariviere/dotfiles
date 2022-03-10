(define-module (baba home gaia)
  #:use-module (baba)
  #:use-module (baba home services browsers)
  #:use-module (baba home services emacs)
  #:use-module (baba home services terminals)
  #:use-module (baba home services wm)
  #:use-module (baba packages security)
  #:use-module (brycus home-service)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home-services wm)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (flat packages emacs))

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

;; TODO: make service for mbsync and notmuch
(home-environment
 (packages
  (list htop isync notmuch bat direnv keychain gnupg))
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
                            (local-file (string-append %channel-root "/etc/fish/config.fish"))))
                          (abbreviations '(("gco" . "git checkout")
                                           ("gs" . "git status")
                                           ("gsr" . "sudo -E guix system reconfigure")
                                           ("ghr" . "guix home reconfigure")
                                           ("cat" . "bat -pp")))))
                (service home-brycus-fish-service-type)
                (service home-git-service-type
                         (home-git-configuration
                          (config
                           `((user
                              ((name . "Bastien Riviere")
                               (email . "babathriviere@gmail.com")
                               (signingKey . "F9B7864F2AB46F18")))
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
                               (include . ,(file-append (package-source foot) "/themes/dracula"))))))))))))
