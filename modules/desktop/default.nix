{ config, lib, pkgs, ... }:

with lib;
let
  unstable = import <unstable> {};
  withLigature = config.services.dotfiles.ligature;
  iosevkaBaba = (unstable.pkgs.iosevka.override {
    privateBuildPlan = {
      design = if withLigature then ["ligset-haskell"] else [] ++ [
        "v-at-fourfold" "v-a-singlestorey" "v-i-zshaped" "v-g-singlestorey"
        "v-l-zshaped" "v-brace-straight" "v-numbersign-slanted" "v-asterisk-hexlow"];
      family = "Iosevka Baba";
    };

    set = "baba";
    # TODO: extraParameters
  });
  iosevkaTermBaba = (unstable.pkgs.iosevka.override {
    privateBuildPlan = {
      design = ["term"
                "v-at-fourfold" "v-a-singlestorey" "v-i-zshaped" "v-g-singlestorey"
                "v-l-zshaped" "v-brace-straight" "v-numbersign-slanted" "v-asterisk-hexlow"];
      family = "Iosevka Term Baba";
    };
    set = "term-baba";
  });

in {
  # options.services.desktop = {
  #   wm = mkOption {
  #     type = types.enum [ "bspwm" ];
  #     default = "bspwm";
  #   };
  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "altgr-intl";
    displayManager.startx.enable = true;
  };

  # TODO: generate with IosevkaGenHS
  fonts.fonts = [
    iosevkaBaba
    iosevkaTermBaba
  ];
}
