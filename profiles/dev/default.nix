{ config, lib, pkgs, usrconf, ... }:

let dotfiles = config.dotfiles;
in {
  documentation.dev.enable = true;
  programs.thefuck.enable = true;

  home-manager.users."${dotfiles.user}" = {
    home.file = { ".editorconfig".source = (usrconf "editorconfig"); };
  };
}
