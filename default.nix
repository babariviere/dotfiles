{ config, lib, options, pkgs, ... }:

let
  dotfiles = config.dotfiles;

  niv = (import (pkgs.fetchFromGitHub {
    owner = "nmattia";
    repo = "niv";
    rev = "49157afd2298749b8a5062fd21079542a6b2de35";
    sha256 = "0q7ymfrcgagcsw6kr93kprag7k358qj8znyzyri53ci1mrsak5y1";
  }) { }).niv;
in {
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    <home-manager/nixos>

    ./themes
    ./modules
  ];

  nix = {
    useSandbox = true;
    gc = {
      automatic = true;
      dates = "*:00:00/2";
      options = "--delete-older-than 7d";
    };
    nixPath = options.nix.nixPath.default ++ [ "config=/etc/dotfiles/config" ];
    autoOptimiseStore = true;
    trustedUsers = [ "root" "@wheel" ];
    binaryCaches = [ "https://aseipp-nix-cache.freetls.fastly.net" ];
  };

  # run gc only if power source is plugged
  systemd.services.nix-gc.unitConfig.ConditionACPower = true;

  nixpkgs = {
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        nur = import (builtins.fetchTarball
          "https://github.com/nix-community/NUR/archive/master.tar.gz") {
            inherit pkgs;
          };
      };
    };
    overlays = [ (import ./pkgs/overlay.nix) ];
  };

  boot = {
    cleanTmpDir = true;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  home-manager.users."${dotfiles.user}".xdg.enable = true;

  environment = {
    variables = {
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_BIN_HOME = "$HOME/.local/bin";
    };

    systemPackages = with pkgs; [
      nixfmt
      nix-prefetch-scripts
      nix-index
      nix-review
      tealdeer
      niv
    ];
  };

  dotfiles.name = "Bastien Rivière";

  time.timeZone = "Europe/Paris";

  users.mutableUsers = false;

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
