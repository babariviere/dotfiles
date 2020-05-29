{ config, lib, pkgs, usrconf, ... }:

let dotfiles = config.dotfiles;
in {
  home-manager.users."${dotfiles.user}" = {
    home.file = { ".editorconfig".source = (usrconf "editorconfig"); };
  };
}
