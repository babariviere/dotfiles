{ config, pkgs, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.supportedFilesystems = [ "zfs" ];
  networking.hostName = "hoenir";
  networking.hostId = "c57c8d79";
  # Network (Hetzner uses static IP assignments, and we don't use DHCP here)
  networking.useDHCP = false;
  networking.interfaces."enp0s31f6".ipv4.addresses = [
    {
      address = "136.243.153.163";
      prefixLength = 24;
    }
  ];
  networking.interfaces."enp0s31f6".ipv6.addresses = [
    {
      address = "2a01:4f8:171:1c03::1";
      prefixLength = 64;
    }
  ];
  networking.defaultGateway = "136.243.153.129";
  networking.defaultGateway6 = { address = "fe80::1"; interface = "enp0s31f6"; };
  networking.nameservers = [ "8.8.8.8" ];
  # Initial empty root password for easy login:
  users.users.root.initialHashedPassword = "";
  services.openssh.permitRootLogin = "prohibit-password";
  users.users.root.openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILhjq6MU3v5OZWhy0kz6XW2z+D2+CRRx1zsdq0eur3p+"];
  services.openssh.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "22.05"; # Did you read the comment?
}
