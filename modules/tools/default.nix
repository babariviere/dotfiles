{ config, lib, pkgs, ... }:

# TODO: move to profile
{
  imports = [ ];

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
