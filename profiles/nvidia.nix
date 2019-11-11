{ config, lib, pkgs, ... }:

{
  # Enable airplane touch
  boot.kernelParams =
    [ "blacklist.nouveau=1" "acpi_osi=!" ''acpi_osi="Windows 2009"'' ];

  hardware.bumblebee.enable = true;
}
