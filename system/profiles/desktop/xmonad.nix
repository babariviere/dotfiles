{ config, pkgs }:

{
  profiles.desktop.xorg.enable = true;

  environment.systemPackages = with pkgs; [ xmobar ];

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
}
