{ config, lib, pkgs, ... }:

{
  imports = [
    ./aws.nix
    ./build.nix
    ./docker.nix
    ./devops.nix
    ./insomnia.nix
    ./light.nix
    ./podman.nix
    ./sql.nix
  ];

  options.dotfiles.tools.virtualisation.enable =
    lib.mkEnableOption "virtualisation";

  config = {
    # Universal tools
    environment.systemPackages = with pkgs; [
      p7zip
      zip
      unzip
      jq
      gzip

      inetutils
      psutils
      pciutils
      usbutils
      lsof

      # Misc utils
      du-dust
    ];

    environment.shellAliases = { du = "dust"; };
  };
}
