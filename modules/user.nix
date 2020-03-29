{ config, lib, pkgs, ... }:

let cfg = config.dotfiles;
in {
  environment.systemPackages = with pkgs; [ xdg-user-dirs ];

  home-manager.users."${cfg.user}" = {
    xdg = {
      enable = true;
      configFile = {
        "nixpkgs/config.nix".text = ''
          {
            allowUnfree = true;
            allowBroken = true;
            android_sdk.accept_license = true;
          }
        '';
      };
      userDirs = {
        enable = true;
        desktop = "$HOME/dsk";
        documents = "$HOME/doc";
        download = "$HOME/dwl";
        music = "$HOME/msc";
        pictures = "$HOME/img";
        publishShare = "$HOME/pub"; # FIXME: will be renamed to publicShare
        templates = "$HOME/tmp";
        videos = "$HOME/vid";
      };
    };
  };
}
