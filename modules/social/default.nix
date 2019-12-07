{ config, lib, pkgs, ... }:

{
  imports = [ ./discord.nix ./keybase.nix ./riot.nix ./slack.nix ];
}
