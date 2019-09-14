{ config, lib, pkgs, ... }:

let
  withLigature = config.services.dotfiles.ligature;
  iosevkaBaba = (pkgs.iosevka.override {
    design = if withLigature then ["ligset-haskell"] else [] ++ [
              "v-at-fourfold" "v-a-singlestorey" "v-i-zshaped" "v-g-singlestorey"
              "v-l-zshaped" "v-brace-straight" "v-numbersign-slanted" "v-asterisk-hexlow"];
    set = "baba";
    family = "Iosevka Baba";
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
  require = [../.];
  # lib.overrideDerivation pkgs.iosevka { attrs: attrs // { preConfigure = '''' } }
  # TODO: generate with IosevkaGenHS
  # TODO: update iosevka.nix to >=2.0
  fonts.fonts = [
    iosevkaBaba
    iosevkaTermBaba
  ];
}
