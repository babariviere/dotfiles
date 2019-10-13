{ config, lib, pkgs, ... }:

with lib;
let
  withLigature = config.services.dotfiles.ligature;
  iosevkaBaba = (pkgs.iosevka.override {
    design = if withLigature then ["ligset-haskell"] else [] ++ [
              "v-at-fourfold" "v-a-singlestorey" "v-i-zshaped" "v-g-singlestorey"
              "v-l-zshaped" "v-brace-straight" "v-numbersign-slanted" "v-asterisk-hexlow"];
    set = "baba";
    family = "Iosevka Baba";
    # TODO: extraParameters
  });
  iosevkaTermBaba = (pkgs.iosevka.override {
    design = ["term"
              "v-at-fourfold" "v-a-singlestorey" "v-i-zshaped" "v-g-singlestorey"
              "v-l-zshaped" "v-brace-straight" "v-numbersign-slanted" "v-asterisk-hexlow"];
    set = "term-baba";
    family = "Iosevka Term Baba";
  });
in
{
  options.services.desktop = {
    wm = mkOption {
      type = types.enum [ "bspwm" ];
      default = "bspwm";
    };
  };

  config = {
    # TODO: generate with IosevkaGenHS
    fonts.fonts = [
      iosevkaBaba
      iosevkaTermBaba
    ];
  };
}
