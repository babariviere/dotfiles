{
  profiles.desktop.libinput.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbVariant = "altgr-intl";
  services.xserver.exportConfiguration = true;
}
