{ config, lib, options, pkgs, ... }:

with lib;

let cfg = config.dotfiles;
in {
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
        nu = "sudo nixos-rebuild switch --upgrade";
        ngc = "nix-collect-garbage -d && sudo nix-collect-garbage -d";
        nrs = "sudo nixos-rebuild switch";
        nrst = "sudo nixos-rebuild switch --show-trace";
        nloc = "nix-locate --top-level";

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
        nix-patch
        tealdeer
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

    time.timeZone = "Europe/Paris";

    users.mutableUsers = false;
  };
}
