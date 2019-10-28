{ config, lib, pkgs, ... }:

let user = "bastien";
in {
  imports = [ ./. ./profiles/msi.nix ];

  dotfiles = {
    user = user;
    email = "bastien.riviere@easy.movie";
    desktop = {
      enable = true;
      i3.enable = true;
      firefox.enable = true;
      polybar.enable = true;
      termite.enable = true;
    };
    dev = { go.enable = true; };
    editors = {
      emacs.enable = true;
      neovim.enable = true;
    };
    shell = {
      direnv.enable = true;
      git.enable = true;
      zsh.enable = true;
    };
    services = { syncthing.enable = true; };
    social = { slack.enable = true; };
    tools = {
      aws.enable = true;
      build.enable = true;
      devops.enable = true;
      insomnia.enable = true;
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
      lib.removeSuffix "\n" (builtins.readFile ./private/bastien.passwd);
  };

  networking.hostName = "sollys";
}
