{ config, lib, pkgs, ... }:

with lib;

let
  generate = import ./generate.nix { inherit lib; };
  cfg = config.services.dotfiles;
  imports =
    [ ./desktop ./dev ./editors ./media ./services ./shell ./social ./tools ];
  options = generate.options imports;
in {
  inherit imports;

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
  } // options;

  config = {
    environment.shellAliases = {
      nix-env = "NIXPKGS_ALLOW_UNFREE=1 nix-env";
      ne = "nix-env";
      nu =
        "sudo nix-channel --update && sudo nixos-rebuild -I config=$HOME/.dotfiles/config switch";
      nre = "sudo nixos-rebuild -I config=$HOME/.dotfiles/config";
      ngc = "nix-collect-garbage -d && sudo nix-collect-garbage -d";
    };

    environment.shellInit = ''
      source .profile
    '';

    home-manager.users."${cfg.user}" = { xdg.enable = true; };
  };
}
