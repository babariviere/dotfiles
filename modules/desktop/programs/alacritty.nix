{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.alacritty;
  colors = dotfiles.theme.colors;
in {
  options.dotfiles.desktop.alacritty.enable = lib.mkEnableOption "alacritty";

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.shellAliases.ssh = "TERM=xterm ssh";

    home-manager.users."${dotfiles.user}" = {
      programs.alacritty = {
        enable = true;

        settings = {
          window.decorations = "none";
          scrolling = {
            history = 10000;
            multiplier = 3;
          };

          font = {
            normal.family = dotfiles.theme.fonts.term.name;
            size = 6.0;
          };

          colors = {
            primary = { inherit (colors) foreground background; };

            normal = dotfiles.theme.normal;
            bright = dotfiles.theme.bright;
          };

          background_opacity = 1.0;

          mouse = {
            hide_when_typing = true;
            url.launcher.program = "xdg-open";
          };

          dynamic_title = true;
        };
      };
    };
  };
}
