{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.initrd.supportedFilesystems = [ "zfs" ];
  boot.supportedFilesystems = [ "zfs" ];

  # TODO: replace uuids
  fileSystems."/" = {
    device = "tank/system";
    fsType = "zfs";
  };

  fileSystems."/nix" = {
    device = "tank/local/nix";
    fsType = "zfs";
  };

  fileSystems."/home/babariviere" = {
    device = "tank/user/babariviere";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/F309-4E12";
    fsType = "vfat";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/58482150-213f-4b15-a10e-b79211a1087c"; }];

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
