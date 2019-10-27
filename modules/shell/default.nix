{ config, lib, pkgs, ... }:

{
  imports = [ ./direnv.nix ./git.nix ./zsh.nix ];
}
