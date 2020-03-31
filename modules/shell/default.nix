{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.shell;
in {
  imports = [ ./direnv.nix ./fish.nix ./git.nix ./starship.nix ./zsh.nix ];

  config = {
    programs.command-not-found.enable = true;
    environment.systemPackages = with pkgs; [ fd exa htop tree ];
  };
}
