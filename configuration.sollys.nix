{ config, lib, pkgs, ... }:

let user = "bastien";
in {
  imports = [ ./. ./private/configuration.sollys.nix ./profiles/nvidia.nix ];

  dotfiles = {
    user = user;
    email = "bastien.riviere@easy.movie";
    theme = "dracula";
    desktop = {
      enable = true;
      i3.enable = true;
      firefox.enable = true;
      polybar.enable = true;
      termite.enable = true;
      compton.enable = false;
      dunst.enable = true;
      rofi.enable = true;
    };
    dev = { go.enable = true; };
    editors = {
      emacs.enable = true;
      neovim.enable = true;
    };
    shell = {
      direnv.enable = true;
      git = {
        enable = true;
        # signingKey = "C45AE603B0DB35266E0E1BBA7014714FCF05D20E";
      };
      zsh.enable = true;
    };
    services = {
      gpg.enable = true;
      ssh.enable = true;
    };
    social = { slack.enable = true; };
    tools = {
      aws.enable = true;
      build.enable = true;
      docker.enable = true;
      devops.enable = true;
      insomnia.enable = true;
      light.enable = true;
      sql.enable = true;
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
  environment.variables = { HOSTNAME = "sollys"; };
}
