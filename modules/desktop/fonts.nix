{ config, lib, pkgs, ... }:

with lib;

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.fonts;
  unstable = import <nixpkgs-unstable> { };

  ## Build iosevka
  mkIosevka = name: set: withLigature:
    unstable.pkgs.iosevka.override {
      privateBuildPlan = {
        design = if withLigature then
          [ "ligset-haskell" ]
        else
          [ "term" ] ++ [
            "v-at-fourfold"
            "v-a-singlestorey"
            "v-i-zshaped"
            "v-g-singlestorey"
            "v-l-zshaped"
            "v-brace-straight"
            "v-numbersign-slanted"
            "v-asterisk-hexlow"
          ];
        family = name;
      };

      inherit set;
      extraParameters =
        if withLigature then builtins.readFile ./iosevka.toml else null;
    };

  iosevkaBaba = mkIosevka "Iosevka Baba" "baba" true;
  iosevkaTermBaba = mkIosevka "Iosevka Term Baba" "term-baba" false;

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
        };
      };
      description = "Font for ${desc}";
      default = {
        name = name;
        package = pkg;
      };
    };
in {
  options.dotfiles.desktop.fonts = {
    term = mkFont "Iosevka Term Baba" iosevkaTermBaba "terminal font";
    mono = mkFont "Iosevka Baba" iosevkaBaba "monospaced font";
    sansSerif = mkFont "Roboto" pkgs.roboto "sans serif font";
    serif = mkFont "Roboto Slab" pkgs.roboto-slab "serif font";

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
