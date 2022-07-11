{ pkgs }:

{
  profiles.desktop.libinput.enable = true;

  environment.systemPackages = with pkgs; [ autorandr ];

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "altgr-intl";
  services.xserver.exportConfiguration = true;
}
