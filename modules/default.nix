{ config, lib, pkgs, ... }:

with lib;

let
  generate = import ./generate.nix { inherit lib; };
  cfg = config.dotfiles;
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
  options = generate.options imports;
in {
  inherit imports;

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
  } // options;

  config = {
    environment.shellAliases = {
      nix-env = "NIXPKGS_ALLOW_UNFREE=1 nix-env";
      ne = "nix-env";
      nu =
        "sudo nix-channel --update && nix build --no-link -f '<nixpkgs/nixos>' config.system.build.toplevel && sudo nixos-rebuild -I config=$HOME/.dotfiles/config switch";
      ngc = "nix-collect-garbage -d && sudo nix-collect-garbage -d";
      ns = "nix-shell";
      nr = "sudo nixos-rebuild";
      nrs =
        "nix build --no-link -f '<nixpkgs/nixos>' config.system.build.toplevel && sudo nixos-rebuild switch";
      nrst = "sudo nixos-rebuild switch --show-trace";
    };

    environment.shellInit = ''
      source $HOME/.profile
    '';

    home-manager.users."${cfg.user}" = { xdg.enable = true; };
  };
}
