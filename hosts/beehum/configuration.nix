{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # System

  time.timeZone = "Europe/Paris";

  ## Boot
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  ## Networking

  networking.hostId = "c25e4f90";
  networking.hostName = "${config.meta.specie.code}";
  networking.domain = "home";

  networking.wireless = {
    enable = true;
    interfaces = [ ];
  };

  ## Profiles

  profiles = { net.tailscale.enable = true; };

  # User

  users.users.babariviere = {
    isNormalUser = true;
    createHome = true;
    extraGroups = [ "wheel" ];
    hashedPassword =
      "$6$hebDRrf7peavZ$fpakn/Inc7A9xAxL5RiZ3WHUcuznSWMC2chOb5bsInISVD3XQjxnark37vQfYY1v32mqkxTfr1Fzj1HUmKj7D1";
  };

  home-manager.users.babariviere = { config, lib, pkgs, ... }: {
    profiles = {
      dev = {
        go.enable = true;
        nix.enable = true;
        rust.enable = true;
      };
      editor = {
        emacs.enable = true;
        editorconfig.enable = true;
      };
      shell = {
        common.enable = true;
        direnv = {
          enable = true;
          nix = true;
        };
        fish.enable = true;
        git.enable = true;
        gh.enable = true;
        tldr.enable = true;
        zsh.enable = true;
      };
    };

    home.stateVersion = "21.03";
  };

  # Meta

  system.stateVersion = "21.05";

  meta = {
    specie = {
      url = "https://ebird.org/species/beehum1";
      code = "beehum1";
      name = "Bee Hummingbird";
    };
  };
}
