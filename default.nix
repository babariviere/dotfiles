{ config, lib, options, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    <home-manager/nixos>
  ];

  nix = {
    nixPath = options.nix.nixPath.default ++ [
      "config=/etc/dotfiles/config"
    ];
    autoOptimiseStore = true;
    trustedUsers = [ "root" "@wheel" ];
  };
  nixpkgs.config.allowUnfree = true;

  boot = {
    cleanTmpDir = true;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  services.dotfiles.name = "Bastien Riviere";
}
