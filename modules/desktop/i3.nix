{ config, lib, pkgs, ... }:

let
  dotfiles = config.services.dotfiles;
in
{
  imports = [
    ./polybar.nix
  ];

  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    configFile = <config/i3>;
    extraPackages = with pkgs; [
      i3lock-color
      betterlockscreen
      dmenu rofi
      feh
    ];
  };

  home-manager.users."${dotfiles.user}".home.file = {
    ".xinitrc".text = ''
#!/bin/sh

exec i3
'';
  };
}
