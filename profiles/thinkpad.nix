{ config, lib, pkgs, ... }:

{
  boot.initrd.availableKernelModules = [ "battery" ];

  hardware.trackpoint.device = "TPPS/2 Elan TrackPoint";

  services.throttled.enable = lib.mkDefault true;
}
