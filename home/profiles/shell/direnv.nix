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
      nix-direnv = lib.mkIf cfg.nix { enable = true; };
    };

    xdg.configFile."direnv/lib/use_asdf.sh".source =
      lib.mkIf cfg.asdf "${config.dotfiles.configDir}/direnv/lib/use_asdf.sh";
  };
}
