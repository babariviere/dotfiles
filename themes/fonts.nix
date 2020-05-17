{ config, lib, pkgs, ... }:

with lib;
let
  dotfiles = config.dotfiles;
  cfg = dotfiles.theme.fonts;

  ## Build options
  mkFont = name: pkg: desc:
    mkOption {
      type = types.submodule {
        options = {
          name = mkOption {
            type = types.str;
            description = "Font name for ${desc}";
            default = name;
          };
          package = mkOption {
            type = types.package;
            description = "Font package for ${desc}";
            default = pkg;
          };
          size = mkOption {
            type = types.integer;
            description = "Font size to use for ${desc}";
            default = 12;
          };
        };
      };
      description = "Font for ${desc}";
      default = {
        name = name;
        package = pkg;
        size = 12;
      };
    };
in {
  # TODO: use an enum for fonts ?
  # TODO: add fontconfig for each font. e.g font size for emacs, rofi and else
  options.dotfiles.theme.fonts = {
    # term = mkFont "Jetbrains Mono" pkgs.unstable.jetbrains-mono
    #   "terminal font (no ligature)";
    #mono = mkFont "Jetbrains Mono" pkgs.unstable.jetbrains-mono "monospaced font";
    term = mkFont "JetBrainsMono Nerd Font"
      (pkgs.unstable.nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
      "term font";
    mono = mkFont "JetBrainsMono Nerd Font"
      (pkgs.unstable.nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
      "monospaced font";
    sansSerif = mkFont "Inter" pkgs.inter "sans serif font";
    serif = mkFont "Noto Serif" pkgs.noto-fonts "serif font";

    ligature = mkEnableOption "ligature";
  };

  config = {
    fonts = {
      fontconfig = {
        enable = true;
        defaultFonts = {
          monospace = [ cfg.mono.name ];
          sansSerif = [ cfg.sansSerif.name ];
          serif = [ cfg.serif.name ];
        };
      };
      fonts = [
        cfg.mono.package
        cfg.term.package
        cfg.sansSerif.package
        cfg.serif.package
        pkgs.nur.repos.babariviere.nerd-font-symbols
      ];
      enableDefaultFonts = true;
    };
  };
}
