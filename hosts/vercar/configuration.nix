{ config, lib, pkgs, ... }: {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "vercar";
  services.tailscale.enable = true;
  networking.firewall.allowedUDPPorts = [ config.services.tailscale.port ];
  networking.firewall.trustedInterfaces =
    [ config.services.tailscale.interfaceName ];

  # Set your time zone.
  time.timeZone = "Europe/Paris";

  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp82s0.useDHCP = true;
  networking.hostId = "461aad11";
  networking.wireless.enable = true;
  networking.wireless.interfaces = [ "wlp82s0" ];

  environment.systemPackages = with pkgs; [ neovim curl wget ];

  virtualisation.docker.enable = false;
  users.users.docker = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "docker" ];
    description = "User with only docker access";
  };
  services.logind.lidSwitch = "ignore";

  user.name = "babariviere";

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.openFirewall = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

  meta = {
    specie = {
      url = "https://ebird.org/species/vercar1";
      code = "vercar";
      name = "Vermilion Cardinal";
    };
  };
}
