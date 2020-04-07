{ config, lib, pkgs, ... }:

{
  boot = {
    # Default boot settings
    cleanTmpDir = true;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    # Boot animation
    plymouth = {
      enable = true;
      theme = "sphere";
      themePackages = [ pkgs.plymouth-themes ];
    };
  };
}
