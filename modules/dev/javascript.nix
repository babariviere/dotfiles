{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.dev.javascript;
in {
  options.dotfiles.dev.javascript.enable = lib.mkEnableOption "javascript";

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [ nodejs yarn ]
      ++ (with nodePackages; [ typescript typescript-language-server ]);

    home-manager.users."${dotfiles.user}" = {
      xdg.configFile = {
        "zsh/rc.d/env.yarn.zsh".source = (usrconf "yarn/env.zsh");
      };
    };
  };
}
