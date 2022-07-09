{ config }:

{
  profiles.desktop.xorg.enable = true;

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
}
