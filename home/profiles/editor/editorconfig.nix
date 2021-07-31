{ config, lib, pkgs, ... }:

{
  home.file.".editorconfig" = {
    source = "${config.dotfiles.configDir}/editorconfig";
  };

  home.packages = [ pkgs.editorconfig-core-c ];
}
