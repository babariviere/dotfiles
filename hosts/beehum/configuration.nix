{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # System

  time.timeZone = "Europe/Paris";

  ## Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  ## Networking

  networking.hostId = "99b75f29";
  networking.hostName = "${config.meta.specie.code}";
  networking.domain = "home";

  networking.useDHCP = false;
  networking.usePredictableInterfaceNames = true;
  networking.useNetworkd = true;
  # networking.interfaces = {
  #   enp4s0.useDHCP = true;
  #   wlp0s20f3.useDHCP = true;
  # };

  systemd.services."systemd-networkd-wait-online".serviceConfig.ExecStart = [
    ""
    "${config.systemd.package}/lib/systemd/systemd-networkd-wait-online --any"
  ];

  systemd.network.enable = true;
  systemd.network.links."00-wlp0s20f3" = {
    enable = true;
    matchConfig.MACAddress = "80:b6:55:ef:fd:a3";
    linkConfig.Name = "wlp0s20f3";
  };

  systemd.network.networks.wlp0s20f3 = {
    name = "wlp0s20f3";
    networkConfig = {
      DHCP = "yes";
      IPv6PrivacyExtensions = "yes";
    };
  };

  networking.wireless.iwd.enable = true;
  networking.wireless.iwd.settings = {
    General = {
      UseDefaultInterface = true;
      EnableNetworkConfiguration = true;
      AddressRandomization = "once";
    };
  };

  ## Profiles

  profiles = { net.tailscale.enable = true; };

  ## Display

  # TODO: configuration
  programs.sway.enable = true;
  services.greetd = {
    enable = true;
    vt = 7;
    settings = {
      default_session.command =
        "${pkgs.greetd.tuigreet}/bin/tuigreet --cmd sway";
    };
  };
  systemd.services.greetd.serviceConfig.Type = "idle";

  # FIXME: what do I need to do this?
  users.users.greeter.group = "greeter";
  users.groups.greeter = { };

  services.xserver.layout = "us";
  services.xserver.xkbVariant = "altgr-intl";

  ## Nix

  ## Audio

  hardware.pulseaudio.enable = true;

  ## Security

  security.pam.loginLimits = [{
    domain = "*";
    type = "-";
    item = "nofile";
    value = "1048576";
  }];

  ## Wayland

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };

  services.pipewire.enable = true;

  # Set XDG portal related variables
  environment.variables.XDG_SESSION_TYPE = "wayland";
  environment.variables.XDG_CURRENT_DESKTOP = "sway";

  ## Virtualisation

  #  virtualisation.containers = {
  #    enable = true;
  #    storage.settings = {
  #      storage.driver = "overlay";
  #      storage.options = {
  #        # Required with zfs, otherwise it won't work
  #        mount_program = "${pkgs.fuse-overlayfs}/bin/fuse-overlayfs";
  #      };
  #    };
  #    containersConf.settings = {
  #      containers.default_ulimits = [ "nofile=1048576:1048576" ];
  #    };
  #  };
  #  virtualisation.podman = {
  #    enable = true;
  #    dockerSocket.enable = true;
  #    dockerCompat = true;
  #  };
  # TODO: find a way to use podman instead
  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
    autoPrune.enable = true;
  };

  services.openvpn.servers = {
    godzilla = {
      config = ''
        client
        dev tun
        proto tcp

        resolv-retry infinite
        nobind
        persist-key
        persist-tun
        mute-replay-warnings
        cipher AES-256-CBC
        comp-lzo
        verb 4

        # Choose one
        #remote 10.208.27.158 1194
        remote 35.205.77.46 1194

        # Configure the paths to your key files
        ca /etc/openvpn/godzilla/ca.crt
        cert /etc/openvpn/godzilla/client.crt
        key /etc/openvpn/godzilla/client.key
      '';
    };
  };

  # User
  users.users.babariviere = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" "docker" "podman" ];
    hashedPassword =
      "$6$hebDRrf7peavZ$fpakn/Inc7A9xAxL5RiZ3WHUcuznSWMC2chOb5bsInISVD3XQjxnark37vQfYY1v32mqkxTfr1Fzj1HUmKj7D1";
    shell = pkgs.fish;
  };

  home-manager.users.babariviere = { config, lib, pkgs, ... }: {
    profiles = {
      dev = {
        go.enable = true;
        nix.enable = true;
        rust.enable = true;
      };
      editor = {
        emacs.enable = true;
        editorconfig.enable = true;
      };
      shell = {
        common.enable = true;
        direnv = {
          enable = true;
          nix = true;
        };
        fish.enable = true;
        git.enable = true;
        gh.enable = true;
        tldr.enable = true;
        zsh.enable = true;
      };
    };

    # TODO: remove this and split in profile
    home.packages = with pkgs; [
      firefox
      conan
      rofi
      mako
      nixos-option
      nyxt
      discord
      slack
      foot
      docker-compose
      neovim
      postgresql
      gcc
      yarn
      poetry
      font-awesome
    ];

    home.sessionPath = [ "~/.local/bin" ];

    programs.waybar = {
      enable = true;
      settings = [{
        layer = "top";
        position = "top";
        height = 30;
        output = [ "eDP-1" "HDMI-A-1" ];
        modules-left = [ "sway/workspaces" "sway/mode" ];
        modules-center = [ "clock" ];
        modules-right = [ "tray" "pulseaudio" "network" "battery" ];
        modules = {
          "sway/mode" = { "format" = ''<span style="italic">{}</span>''; };
          network = {
            interface = "wl*";
            format-wifi = "{essid} ({signalStrength}%) ";
            format-ethernet = "{ifname} ";
            format-disconnected = "";
            max-length = 50;
            # "on-click" = "foot -e 'nmtui'";
          };
          tray = {
            icon-size = 15;
            spacing = 10;
          };
          battery = {
            states = {
              good = 95;
              warning = 20;
              critical = 10;
            };
            format = "{capacity}% {icon}";
            format-charging = "{capacity}% ";
            format-plugged = "{capacity}% ";
            format-alt = "{time} {icon}";
            format-full = "";
            format-icons = [ "" "" "" "" "" ];
          };
          pulseaudio = {
            format = "{volume}% {icon} ";
            format-bluetooth = "{volume}% {icon} {format_source}";
            format-bluetooth-muted = " {icon} {format_source}";
            format-muted = " ";
            format-source = "{volume}% ";
            format-source-muted = "";
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = [ "" "" "" ];
            };
            # "on-click" = "pavucontrol";
          };
        };
      }];
      style = ''
        * {
            font-family: all-the-icons, MonoLisa;
            font-size: 13px;
            border: none;
            border-radius: 0;
            min-height: 0;
            box-shadow:    none;
            text-shadow:   none;
            transition-duration: 0s;
        }

        window {
            color:      rgba(217, 216, 216, 1);
            background: rgba(35, 31, 32, 0.00);
        }

        window#waybar.solo {
            color:      rgba(217, 216, 216, 1);
            background: rgba(35, 31, 32, 0.85);
        }

        #workspaces {
            margin: 0 5px;
        }

        #workspaces button {
            padding:    0 5px;
            color:      rgba(217, 216, 216, 0.4);
        }

        #workspaces button.visible {
            color:      rgba(217, 216, 216, 1);
        }

        #workspaces button.focused {
            border-top: 3px solid rgba(217, 216, 216, 1);
            border-bottom: 3px solid rgba(217, 216, 216, 0);
        }

        #workspaces button.urgent {
            color:      rgba(238, 46, 36, 1);
        }

        #mode, #battery, #cpu, #memory, #network, #pulseaudio, #idle_inhibitor, #backlight, #custom-storage, #custom-spotify, #custom-weather, #custom-mail {
            margin:     0px 6px 0px 10px;
            min-width:  25px;
        }

        #clock {
            margin:     0px 16px 0px 10px;
            min-width:  140px;
        }

        #battery.warning {
        color:       rgba(255, 210, 4, 1);
        }

        #battery.critical {
            color:      rgba(238, 46, 36, 1);
        }

        #battery.charging {
            color:      rgba(217, 216, 216, 1);
        }

        #pulseaudio.muted {
            font-family: github-octicons;
        }
      '';
    };

    wayland.windowManager.sway = {
      enable = true;
      xwayland = true;
      wrapperFeatures.gtk = true;
      config = {
        bars = [{
          position = "top";
          command = "${pkgs.waybar}/bin/waybar";
        }];
        focus.followMouse = false;
        fonts = {
          names = [ "MonoLisa" ];
          style = "Regular";
          size = 10.0;
        };
        input = {
          "*" = {
            xkb_layout = "us";
            xkb_variant = "altgr-intl";
          };
          "1267:13:Elan_Touchpad" = {
            click_method = "clickfinger";
            dwt = "enabled";
            natural_scroll = "enabled";
            tap = "disabled";
          };
        };
        output = let
          artworks = pkgs.fetchFromGitHub {
            owner = "NixOS";
            repo = "nixos-artwork";
            rev = "9bd73014f75c2ce97d104c78314d78eb2493e24d";
            sha256 = "1976vc2w26h78wngqm7q1rnqa387y1bpf9wicxfghbc2qkimq2m5";
          };
          bg = "${artworks}/wallpapers/nix-wallpaper-dracula.png fill";
        in {
          eDP-1 = {
            pos = "0 0";
            inherit bg;
          };
          HDMI-A-1 = {
            pos = "1920 0";
            inherit bg;
          };
        };
        modifier = "Mod4";
        terminal = "${pkgs.foot}/bin/foot";
        keybindings = lib.mkOptionDefault {
          "Mod4+End" =
            "exec ${pkgs.swaylock-effects}/bin/swaylock --screenshots --clock --effect-blur 7x5 --effect-vignette 0.5:0.5 --grace 2 --fade-in 0.2";
          "XF86AudioRaiseVolume" = "exec ${pkgs.pamixer}/bin/pamixer -i 2";
          "XF86AudioLowerVolume" = "exec ${pkgs.pamixer}/bin/pamixer -d 2";
          "XF86AudioMute" = "exec ${pkgs.pamixer}/bin/pamixer -t";
          "XF86MonBrightnessDown" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s 5%-";
          "XF86MonBrightnessUp" =
            "exec ${pkgs.brightnessctl}/bin/brightnessctl s +5%";
        };
        startup = [
          {
            # Import variables needed for screen sharing and gnome3 pinentry to work.
            command =
              "${pkgs.dbus}/bin/dbus-update-activation-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway";
          }
          {
            command =
              "${pkgs.systemd}/bin/systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP";
          }

        ];
      };
      extraConfig = ''
        default_border pixel 2
      '';
    };

    programs.foot = {
      enable = true;
      # Broken for me
      server.enable = false;
      settings = {
        main = {
          term = "xterm-256color";
          font = "MonoLisa:size=10";
          dpi-aware = "no";
          include = "${pkgs.foot.src}/themes/dracula";
        };
      };
    };

    # TODO: refactor me
    programs.git = {
      extraConfig = {
        user.signingKey = "F9B7864F2AB46F18";
        commit.gpgSign = true;
      };
      includes = [{
        condition = "gitdir:~/src/gitlab.com/TankerHQ/";
        contents = {
          user = {
            email = "bastien.riviere@tanker.io";
            signingKey = "ACFD416C8BFB251A";
          };
          commit.gpgSign = true;
        };
      }];
    };

    programs.gpg.enable = true;

    services.gpg-agent = {
      enable = true;
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
    };

    home.stateVersion = "21.03";
  };

  # Meta

  system.stateVersion = "21.11";

  meta = {
    specie = {
      url = "https://ebird.org/species/beehum1";
      code = "beehum";
      name = "Bee Hummingbird";
    };
  };
}
