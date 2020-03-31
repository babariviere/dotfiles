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
      userDirs = let dir = path: "$HOME/usr/${path}";
      in {
        enable = true;
        desktop = dir "dsk";
        documents = dir "doc";
        download = dir "dwl";
        music = dir "msc";
        pictures = dir "img";
        publicShare = dir "pub";
        templates = dir "tmp";
        videos = dir "vid";
      };
    };
  };
}
