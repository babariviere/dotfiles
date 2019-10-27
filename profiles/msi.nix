{ config, lib, pkgs, ... }:

{
  # Enable airplane touch
  boot.kernelParams = [ "acpi_osi=!" ''acpi_osi="Windows 2009"'' ];

  hardware.bumblebee.enable = true;
}
