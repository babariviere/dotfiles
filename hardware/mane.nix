{ config, lib, pkgs, ... }:

{
  boot.initrd.availableKernelModules =
    [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ] ++ [ config.boot.kernelPackages.acpi_call ];

  networking.hostId = "0e14d244";

  boot.initrd.luks.devices = {
    nixos-0.device = "/dev/disk/by-uuid/7b93bae9-8358-47e8-84ea-05c4baa69683";
    nixos-1.device = "/dev/disk/by-uuid/41f61f4d-1fef-449b-a078-74fa9f62bd0d";
  };

  fileSystems."/" = {
    device = "nixos/root";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/C05A-8465";
    fsType = "vfat";
  };

  fileSystems."/nix" = {
    device = "nixos/nix";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "nixos/home";
    fsType = "zfs";
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/db81e541-3dbe-4dca-a4e2-bea16ed62325"; }];

  nix.maxJobs = lib.mkDefault 16;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
}
