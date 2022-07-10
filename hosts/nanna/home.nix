{ config, pkgs, ... }:

{
  home.username = "babariviere";
  home.homeDirectory = "/home/babariviere";

  targets.genericLinux.enable = true;
  programs.bash.enable = true;
  programs.git = {
    extraConfig = {
      user.signingKey = "39035CC0B75D1142";
      commit.gpgSign = true;
    };
  };

  profiles = {
    desktop.alacritty.enable = true;
    dev.haskell.enable = true;
    editor.emacs.enable = true;
    shell = {
      common.enable = true;
      direnv = {
        enable = true;
        nix = true;
        asdf = true;
      };
      git.enable = true;
      niv.enable = true;
      zsh.enable = true;
    };
  };

  home.stateVersion = "22.05";
  programs.home-manager.enable = true;
}
