{ config, lib, pkgs, ... }:

with lib;
let
  files = builtins.readDir ./.;
  themes = filterAttrs (n: v: v == "directory") files;

  injectAnsi = colors:
    {
      color0 = colors.black or (throw "black color is not defined");
      color1 = colors.red or (throw "red color is not defined");
      color2 = colors.green or (throw "green color is not defined");
      color3 = colors.yellow or (throw "yellow color is not defined");
      color4 = colors.blue or (throw "blue color is not defined");
      color5 = colors.magenta or (throw "magenta color is not defined");
      color6 = colors.cyan or (throw "cyan color is not defined");
      color7 = colors.white or (throw "white color is not defined");
      color8 = colors.lightBlack or colors.black;
      color9 = colors.lightRed or colors.red;
      color10 = colors.lightGreen or colors.green;
      color11 = colors.lightYellow or colors.yellow;
      color12 = colors.lightBlue or colors.blue;
      color13 = colors.lightMagenta or colors.magenta;
      color14 = colors.lightCyan or colors.cyan;
      color15 = colors.lightWhite or colors.white;
    } // colors;
in {
  options.dotfiles = {
    theme = mkOption {
      type = types.enum (builtins.attrNames themes);
      description = "Define theme to use.";
    };
    colors = mkOption {
      type = types.attrsOf (types.strMatching "#[a-f0-9]{6}");
      description = "Colors defined by theme. This will be overrided by theme.";
    };

    doomTheme = mkOption {
      type = types.str;
      description = "Emacs Doom theme.";
    };

    wallpaper = mkOption {
      type = types.path;
      description =
        "Wallapper defined by theme. This will be overrided by theme.";
    };
  };

  config = {
    dotfiles.colors =
      mkForce (injectAnsi (import (./. + "/${config.dotfiles.theme}")));

    dotfiles.doomTheme = "doom-${config.dotfiles.theme}";
    dotfiles.wallpaper = mkForce (./. + "/${config.dotfiles.theme}/wallpaper");
  };
}
