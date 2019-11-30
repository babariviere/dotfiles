{ config, lib, pkgs, ... }:

{
  boot.kernelParams = [ "blacklist.nouveau=1" ];

  hardware.bumblebee = {
    enable = true;
    connectDisplay = true;
  };

  nixpkgs.overlays = [
    (self: super: {
      bumblebee = super.bumblebee.override {
        extraNvidiaDeviceOptions = ''
          Option "AllowEmptyInitialConfiguration"
        '';
      };
    })
  ];
}
