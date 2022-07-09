{ pkgs, config, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];

  # System

  time.timeZone = "Europe/Paris";

  ## Networking

  networking.hostName = "loki";
  networking.domain = "home";

  networking.wireless = {
    enable = true;
    interfaces = [ "wlp82s0" ];
    allowAuxiliaryImperativeNetworks = true;
  };

  ## Hardware

  hardware.bluetooth = {
    enable = true;
    settings = { General = { ControllerMode = "dual"; }; };
  };

  ## Virtualisation

  virtualisation.lxd = {
    enable = true;
    recommendedSysctlSettings = true;
  };

  ## Profiles

  profiles = {
    desktop = {
      xmonad.enable = true;
      tlp.enable = true;
      libinput.enable = true;
      nvidia.enable = true;
    };
    media.pipewire.enable = true;
  };

  ## Display

  services.xserver.displayManager.lightdm.enable = true;

  ## User

  users.users.babariviere = {
    isNormalUser = true;
    createHome = true;
    extraGroups =
      [ "wheel" "docker" "podman" "adbusers" "shadow" "kvm" "libvirt" "lxd" "video" "input" ];
    shell = pkgs.zsh;
  };

  home-manager.users.babariviere = { config, lib, pkgs, ... }: {
    profiles = {
      dev = {
        cc.enable = true;
        nix.enable = true;
      };
      desktop.xmonad.enable = true;
      editor = {
        emacs.enable = true;
        editorconfig.enable = true;
      };
      shell = {
        common.enable = true;
        direnv = {
          enable = true;
          nix = true;
          asdf = true;
        };
        git.enable = true;
        tldr.enable = true;
        zsh.enable = true;
      };
    };

    # TODO: move me to my own profile
    programs.git = {
      extraConfig = {
        user.signingKey = "39035CC0B75D1142";
        commit.gpgSign = true;
      };
    };

    programs.gpg.enable = true;

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      extraConfig = ''
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
      defaultCacheTtl = 1800;
    };

    home.sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];
    home.stateVersion = "22.05";
  };
}
