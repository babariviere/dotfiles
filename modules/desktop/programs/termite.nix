{ config, lib, pkgs, usrconf, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.termite;
  colors = dotfiles.theme.colors;
in {
  options.dotfiles.desktop.termite.enable = lib.mkEnableOption "termite";

  # TODO: if we want support for emacs colors, we need to add this: https://github.com/syl20bnr/spacemacs/wiki/Terminal
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    environment.systemPackages = with pkgs; [ termite xterm-24bits ];
    environment.shellAliases = { ssh = "TERM=xterm ssh"; };

    home-manager.users."${dotfiles.user}" = {
      programs.termite = {
        enable = true;

        ## Options
        audibleBell = false;
        clickableUrl = true;
        filterUnmatchedUrls = true;
        font = "${dotfiles.theme.fonts.term.name} 11";
        scrollbackLines = 1000;
        sizeHints = true;

        ## Colors
        foregroundColor = colors.foreground;
        foregroundBoldColor = colors.foregroundBold;
        cursorColor = colors.cursor;
        cursorForegroundColor = colors.cursorForeground;
        backgroundColor = colors.background;

        colorsExtra = ''
          # black
          color0  = ${colors.color0}
          color8  = ${colors.color8}

          # red
          color1  = ${colors.color1}
          color9  = ${colors.color9}

          # green
          color2  = ${colors.color2}
          color10 = ${colors.color10}

          # yellow
          color3  = ${colors.color3}
          color11 = ${colors.color11}

          # blue
          color4  = ${colors.color4}
          color12 = ${colors.color12}

          # magenta
          color5  = ${colors.color5}
          color13 = ${colors.color13}

          # cyan
          color6  = ${colors.color6}
          color14 = ${colors.color14}

          # white
          color7  = ${colors.color7}
          color15 = ${colors.color15}
        '';
      };

      xdg.configFile = {
        "zsh/rc.d/env.termite.zsh".source = usrconf "termite/env.zsh";
        "fish/rc.d/env.termite.fish".source = usrconf "termite/env.fish";
      };
    };
  };
}
