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

  # User
  users.users.babariviere = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" ];
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
    ];

    programs.waybar = {
      enable = true;
      settings = [{
        layer = "top";
        position = "top";
        height = 30;
        output = [ "eDP-1" "HDMI-A-1" ];
        modules-left = [ "sway/workspaces" "sway/mode" "wlr/taskbar" ];
        modules-center = [ "sway/window" ];
        modules-right = [ "network" "pulseaudio" ];
        modules = {
          "sway/workspaces" = {
            disable-scroll = true;
            all-outputs = true;
          };
        };
      }];
      style = ''
        * {
            font-family: MonoLisa;
            font-size: 10px;
        }
        window#waybar {
            background: rgba(43, 48, 59, 0.5);
            border-bottom: 3px solid rgba(100, 114, 125, 0.5);
            color: white;
        }

        tooltip {
          background: rgba(43, 48, 59, 0.5);
          border: 1px solid rgba(100, 114, 125, 0.5);
        }
        tooltip label {
          color: white;
        }

        #workspaces button {
            padding: 0 5px;
            background: transparent;
            color: white;
            border-bottom: 3px solid transparent;
        }

        #workspaces button.focused {
            background: #64727D;
            border-bottom: 3px solid white;
        }

        #mode, #clock, #battery {
            padding: 0 10px;
            margin: 0 5px;
        }

        #mode {
            background: #64727D;
            border-bottom: 3px solid white;
        }

        #clock {
            background-color: #64727D;
        }

        #battery {
            background-color: #ffffff;
            color: black;
        }

        #battery.charging {
            color: white;
            background-color: #26A65B;
        }

        @keyframes blink {
            to {
                background-color: #ffffff;
                color: black;
            }
        }

        #battery.warning:not(.charging) {
            background: #f53c3c;
            color: white;
            animation-name: blink;
            animation-duration: 0.5s;
            animation-timing-function: linear;
            animation-iteration-count: infinite;
            animation-direction: alternate;
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
        output = {
          eDP-1 = { pos = "0 0"; };
          HDMI-A-1 = { pos = "1920 0"; };
        };
        modifier = "Mod4";
        terminal = "${pkgs.foot}/bin/foot";
        keybindings = lib.mkOptionDefault {
          "Mod4+End" =
            "exec ${pkgs.swaylock-effects}/bin/swaylock --screenshots --clock --effect-blur 7x5 --effect-vignette 0.5:0.5 --grace 2 --fade-in 0.2";
          "XF86AudioRaiseVolume" = "exec ${pkgs.pamixer}/bin/pamixer -i 2";
          "XF86AudioLowerVolume" = "exec ${pkgs.pamixer}/bin/pamixer -d 2";
          "XF86AudioMute" = "exec ${pkgs.pamixer}/bin/pamixer -t";
        };
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
