{ config, lib, pkgs, ... }:

with lib;

let cfg = config.dotfiles;
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
    environment.shellAliases = {
      nix-env = "NIXPKGS_ALLOW_UNFREE=1 nix-env";
      ne = "nix-env";
      nu = "sudo nix-channel --update && sudo nixos-rebuild switch";
      ngc = "nix-collect-garbage -d && sudo nix-collect-garbage -d";
      ns = "nix-shell";
      nr = "sudo nixos-rebuild";
      nrs = "sudo nixos-rebuild switch";
      nrst = "sudo nixos-rebuild switch --show-trace";
    };

    environment.shellInit = ''
      source $HOME/.profile
    '';

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
  };
}
