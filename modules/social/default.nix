{ config, lib, pkgs, ... }:

{
  imports = [ ./discord.nix ./slack.nix ];
}
