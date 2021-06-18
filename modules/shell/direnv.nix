{ config, lib, pkgs, ... }:

let cfg = config.my.shell.direnv;
in {
  options.my.shell.direnv = with lib; {
    enable = mkEnableOption "direnv";

    nix = mkEnableOption "nix-direnv";
    asdf = mkEnableOption "asdf";
  };

  config = lib.mkIf cfg.enable {
    hm.programs.direnv = {
      enable = true;
      nix-direnv = lib.mkIf cfg.nix {
        enable = true;
        enableFlakes = true;
      };
    } // (lib.optionalAttrs cfg.asdf {
      stdlib = builtins.readFile "${config.dotfiles.configDir}/direnvrc";
    });
  };
}
