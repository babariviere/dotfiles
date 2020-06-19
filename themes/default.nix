{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  files = builtins.readDir ./.;
  themes = filterAttrs (n: v: v == "directory") files;

  ansi = colors: {
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
  };

  bright = colors: {
    black = colors.lightBlack or colors.black;
    red = colors.lightRed or colors.red;
    green = colors.lightGreen or colors.green;
    yellow = colors.lightYellow or colors.yellow;
    blue = colors.lightBlue or colors.blue;
    magenta = colors.lightMagenta or colors.magenta;
    cyan = colors.lightCyan or colors.cyan;
    white = colors.lightWhite or colors.white;
  };

  normal = colors: {
    black = colors.black or (throw "black color is not defined");
    red = colors.red or (throw "red color is not defined");
    green = colors.green or (throw "green color is not defined");
    yellow = colors.yellow or (throw "yellow color is not defined");
    blue = colors.blue or (throw "blue color is not defined");
    magenta = colors.magenta or (throw "magenta color is not defined");
    cyan = colors.cyan or (throw "cyan color is not defined");
    white = colors.white or (throw "white color is not defined");
  };

  theme = (import (./. + "/${config.dotfiles.theme.name}"));
in {
  imports = [ ./fonts.nix ];

  options.dotfiles.theme = {
    name = mkOption {
      type = types.enum (builtins.attrNames themes);
      description = "Define theme to use.";
    };
    colors = mkOption {
      type = types.attrsOf (types.strMatching "#[a-f0-9]{6}");
      description = "Colors defined by theme.";
      readOnly = true;
    };

    colorsAnsi = mkOption {
      type = types.attrsOf (types.strMatching "#[a-f0-9]{6}");
      description = "Colors defined by theme (ANSI format).";
      readOnly = true;
    };

    bright = mkOption {
      type = types.attrsOf (types.strMatching "#[a-f0-9]{6}");
      description = "Bright colors defined by theme.";
      readOnly = true;
    };

    normal = mkOption {
      type = types.attrsOf (types.strMatching "#[a-f0-9]{6}");
      description = "Normal colors defined by theme.";
      readOnly = true;
    };

    doom = mkOption {
      type = types.str;
      description = "Emacs Doom theme.";
    };

    wallpaper = mkOption {
      type = types.path;
      description = "Wallapper defined by theme.";
      readOnly = true;
    };
  };

  # TODO: configure fonts with theme ?
  config.dotfiles.theme = {
    colors = (ansi theme // theme);

    colorsAnsi = (ansi theme // {
      foreground = theme.foreground;
      background = theme.background;
    });

    bright = bright theme;
    normal = normal theme;

    doom = "doom-${config.dotfiles.theme.name}";
    wallpaper = let
      path = (./. + "/${config.dotfiles.theme.name}/wallpaper.nix");
      expr = pkgs.callPackage path { };
    in mkForce expr.src;
  };
}
