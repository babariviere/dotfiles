{ config, lib, pkgs, ... }:

{
  imports = [ ./direnv.nix ./git.nix ./starship.nix ./zsh.nix ];
}
