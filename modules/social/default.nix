{ config, lib, pkgs, ... }:

{
  imports = [ ./discord.nix ./riot.nix ./slack.nix ];
}
