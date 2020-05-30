{ config, lib, pkgs, ... }:

{
  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  hardware.enableRedistributableFirmware = true;

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/2186d832-dc52-4ef7-b7c2-35ac6887586a";
      preLVM = true;
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/3df021c1-a84d-4338-89a6-55e60957b233";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/4674-2D1A";
    fsType = "vfat";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/26eee295-2a46-41bf-919b-d6895456ccd5"; }];

  nix.maxJobs = lib.mkDefault 12;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
