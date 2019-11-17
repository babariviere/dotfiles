{ config, lib, pkgs, ... }:

let user = "babariviere";
in {
  imports = [ ./. ./profiles/nvidia.nix ];

  dotfiles = {
    user = user;
    email = "babathriviere@gmail.com";
    theme = "dracula";
    desktop = {
      enable = true;
      i3.enable = true;
      firefox.enable = true;
      polybar.enable = true;
      termite.enable = true;
      compton.enable = false;
      rofi.enable = true;
    };
    dev = {
      haskell.enable = true;
      rust.enable = true;
    };
    editors = {
      emacs.enable = true;
      neovim.enable = true;
    };
    media = {
      plex.enable = true;
      spotify.enable = true;
    };
    shell = {
      direnv.enable = true;
      git = {
        enable = true;
        signingKey = "6BCFEDF322EB0040B5FC4296EECF965F5AAA4E1A";
      };
      zsh.enable = true;
    };
    services = {
      gpg.enable = true;
      ssh.enable = true;
      syncthing.enable = true;
    };
    social = {
      discord.enable = true;
      riot.enable = true;
    };
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
  environment.variables = { HOSTNAME = "mane"; };
}
