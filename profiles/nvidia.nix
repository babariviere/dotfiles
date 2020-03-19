{ config, lib, pkgs, ... }:

{
  boot.kernelParams = [ "blacklist.nouveau=1" ];

  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.nvidia.optimus_prime = {
    enable = true;
    nvidiaBusId = "PCI:1:0:0";
    intelBusId = "PCI:0:2:0";
  };

  environment.systemPackages = with pkgs; [ prime ];
}
