{ config, lib, pkgs, ... }:

{
  # TODO: move to profiles
  config = {
    programs.command-not-found.enable = false;
    environment.systemPackages = with pkgs; [ fd exa htop tree ];
  };
}
