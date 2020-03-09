{ config, lib, options, pkgs, ... }:

with lib;

let
  cfg = config.dotfiles;

  niv = (import (pkgs.fetchFromGitHub {
    owner = "nmattia";
    repo = "niv";
    rev = "49157afd2298749b8a5062fd21079542a6b2de35";
    sha256 = "0q7ymfrcgagcsw6kr93kprag7k358qj8znyzyri53ci1mrsak5y1";
  }) { }).niv;
in {
  imports = [
    ./desktop
    ./dev
    ./editors
    ./media
    ./network.nix
    ./services
    ./shell
    ./social
    ./tools
  ];

  options.dotfiles = {
    user = mkOption {
      type = types.str;
      description = "User used for services";
    };

    email = mkOption {
      type = types.str;
      description = "Email of the user";
    };

    name = mkOption {
      type = types.str;
      description = "Name of the user";
    };
  };

  config = {
    environment = {
      shellAliases = {
        # Nix commands
        nix-env = "NIXPKGS_ALLOW_UNFREE=1 nix-env";
        ne = "nix-env";
        nu = "sudo nix-channel --update && sudo nixos-update switch";
        ngc = "nix-collect-garbage -d && sudo nix-collect-garbage -d";
        ns = "nix-shell";
        nr = "sudo nixos-update";
        nrs = "sudo nixos-update switch";
        nrst = "sudo nixos-updat switch --show-trace";

        # Utilities
        cat = "bat";
      };

      shellInit = ''
        source $HOME/.profile
      '';

      systemPackages = with pkgs; [
        nixfmt
        nix-prefetch-scripts
        nix-index
        nix-review
        nixos-update
        nix-patch
        tealdeer
        niv
        file
        bat # cat with more power
        cachix
      ];

      variables = {
        # XDG configuration
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_CACHE_HOME = "$HOME/.cache";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_BIN_HOME = "$HOME/.local/bin";

        # Utilities
        MANPAGER = "/bin/sh -c 'col -bx | ${pkgs.bat}/bin/bat -l map -p'";
        BAT_THEME = "TwoDark";
      };
    };

    home-manager.users."${cfg.user}" = {
      xdg.enable = true;
      xdg.configFile = {
        "nixpkgs/config.nix".text = ''
          {
            allowUnfree = true;
            allowBroken = true;
            android_sdk.accept_license = true;
          }
        '';
      };
    };

    nix = {
      useSandbox = true;
      gc = {
        automatic = true;
        dates = "*-*-* 18:00:00";
        options = "--delete-older-than 7d";
      };
      nixPath = options.nix.nixPath.default
        ++ [ "config=/etc/dotfiles/config" ];
      autoOptimiseStore = true;
      trustedUsers = [ "root" "@wheel" ];
      binaryCaches = [ "https://aseipp-nix-cache.freetls.fastly.net" ];
    };
    # run gc only if power source is plugged
    systemd.services.nix-gc.unitConfig.ConditionACPower = true;

    nixpkgs = {
      config = {
        allowUnfree = true;
        packageOverrides = pkgs: {
          nur = import (builtins.fetchTarball
            "https://github.com/nix-community/NUR/archive/master.tar.gz") {
              inherit pkgs;
            };
        };
      };
      overlays = [ (import ../pkgs/overlay.nix) ];
    };

    # Default boot configuration
    boot = {
      cleanTmpDir = true;
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };
    };

    time.timeZone = "Europe/Paris";

    users.mutableUsers = false;

    # TODO: auto git clone

    # This value determines the NixOS release with which your system is to be
    # compatible, in order to avoid breaking some software such as database
    # servers. You should change this only after NixOS release notes say you
    # should.
    system.stateVersion = "19.09"; # Did you read the comment?
    system.autoUpgrade = {
      enable = true;
      channel = "https://nixos.org/channels/nixos-19.09";
    };
    systemd.services.nixos-upgrade.unitConfig.ConditionACPower = true;
    systemd.timers.nixos-upgrade.timerConfig.Persistent = true;
  };
}
