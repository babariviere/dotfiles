{ config, lib, pkgs, ... }:

let user = "babariviere";
in {
  imports = [ ./. ./profiles/msi.nix ];

  dotfiles = {
    user = user;
    email = "babathriviere@gmail.com";
    desktop = {
      enable = true;
      i3.enable = true;
      firefox.enable = true;
      polybar.enable = true;
      termite.enable = true;
      compton.enable = true;
    };
    dev = { rust.enable = true; };
    editors = {
      emacs.enable = true;
      neovim.enable = true;
    };
    media = { plex.enable = true; };
    shell = {
      direnv.enable = true;
      git.enable = true;
      zsh.enable = true;
    };
    services = { syncthing.enable = true; };
    social = { discord.enable = true; };
    tools = {
      build.enable = true;
      devops.enable = true;
      light.enable = true;
    };
  };

  networking.wireless = {
    enable = true;
    networks = import ./private/networks.nix;
  };

  users.users."${user}" = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" "video" ];
    hashedPassword =
      lib.removeSuffix "\n" (builtins.readFile ./private/babariviere.passwd);
  };

  networking.hostName = "mane";
}
