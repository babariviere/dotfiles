{ config, lib, options, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    <home-manager/nixos>

    ./modules
  ];

  nix = {
    useSandbox = true;
    gc = {
      automatic = true;
      dates = "*:0/30";
      options = "--delete-older-than 7d";
    };
    nixPath = options.nix.nixPath.default ++ [ "config=/etc/dotfiles/config" ];
    autoOptimiseStore = true;
    trustedUsers = [ "root" "@wheel" ];
  };

  # run gc only if power source is plugged
  systemd.services.nix-gc.unitConfig.ConditionACPower = true;

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur = import (builtins.fetchTarball
        "https://github.com/nix-community/NUR/archive/master.tar.gz") {
          inherit pkgs;
        };
    };
  };

  boot = {
    cleanTmpDir = true;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  environment.systemPackages = with pkgs; [
    nixfmt
    nix-prefetch-scripts
    killall
    # sorry, I have to
    lolcat
  ];

  dotfiles.name = "Bastien Rivière";

  time.timeZone = "Europe/Paris";

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
  system.autoUpgrade = {
    enable = true;
    channel = "https://nixos.org/channels/nixos-19.09";
    # TODO: set date ?
  };
  systemd.services.nixos-upgrade.unitConfig.ConditionACPower = true;
  systemd.timers.nixos-upgrade.timerConfig.Persistent = true;
}
