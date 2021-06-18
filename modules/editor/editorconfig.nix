{ config, lib, pkgs, ... }:

let cfg = config.my.editor.editorconfig;
in {
  options.my.editor.editorconfig = with lib; {
    enable = mkEnableOption "editorconfig";
  };

  config = lib.mkIf cfg.enable {
    home.file.".editorconfig" = {
      source = "${config.dotfiles.configDir}/editorconfig";
    };

    home.packages = [ pkgs.editorconfig-core-c ];
  };
}
