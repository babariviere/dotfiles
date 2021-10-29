{ config, lib, ... }:

let cfg = config.profiles.shell.direnv;
in {
  options = {
    nix = lib.mkEnableOption "nix-direnv";
    asdf = lib.mkEnableOption "asdf";
  };

  config = {
    programs.direnv = {
      enable = true;
      nix-direnv = lib.mkIf cfg.nix {
        enable = true;
        enableFlakes = true;
      };
    } // {
      stdlib = builtins.readFile "${config.dotfiles.configDir}/direnvrc";
    };
  };
}
