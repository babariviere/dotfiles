{ config, lib, pkgs, ... }:

with lib;
{
  options.services.dotfiles = {
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

    ligature = mkOption {
      type = types.bool;
      description = "Add ligature support to all services";
      default = true;
    };
  };

  config = {
    environment.shellAliases = {
      nix-env = "NIXPKGS_ALLOW_UNFREE=1 nix-env";
      ne = "nix-env";
      nu = "sudo nix-channel --update && sudo nixos-rebuild -I config=$HOME/.dotfiles/config switch";
      nre = "sudo nixos-rebuild -I config=$HOME/.dotfiles/config";
      ngc = "nix-collect-garbage -d && sudo nix-collect-garbage -d";
    };

    home-manager.users."${cfg.user}" = {
      xdg.enable = true;
    };
  };
}
