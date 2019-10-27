{ config, lib, options, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    <home-manager/nixos>

    ./modules
  ];

  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
    nixPath = options.nix.nixPath.default ++ [ "config=/etc/dotfiles/config" ];
    autoOptimiseStore = true;
    trustedUsers = [ "root" "@wheel" ];
  };
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      nur = import (builtins.fetchTarball
        "https://github.com/nix-community/NUR/archive/master.tar.gz") {
          inherit pkgs;
          repoOverrides = {
            # babariviere = import (builtins.fetchTarball
            #   "https://github.com/babariviere/nur-packages/archive/master.tar.gz") {
            #     inherit pkgs;
            #   };
            babariviere =
              import /home/babariviere/projects/nur-packages { inherit pkgs; };
          };
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

  environment.systemPackages = with pkgs; [ nixfmt nix-prefetch-scripts ];

  services.dotfiles.name = "Bastien Rivière";
}
