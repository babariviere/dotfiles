{ config, lib, pkgs, ... }:

{
  imports = [ ./discord.nix ./keybase.nix ./riot.nix ./signal.nix ./slack.nix ];
}
