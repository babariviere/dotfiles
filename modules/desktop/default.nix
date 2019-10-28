{ config, lib, pkgs, ... }:

with lib;
let
  # unstable =  import <nixpkgs-unstable> {};
  # TODO: remove me
  unstable = import (builtins.fetchTarball
    "https://github.com/babariviere/nixpkgs/archive/iosevka-v2.3.2.tar.gz") { };
  cfg = config.dotfiles.desktop;
  withLigature = cfg.ligature;
  iosevkaBaba = (unstable.pkgs.iosevka.override {
    privateBuildPlan = {
      design = if withLigature then
        [ "ligset-haskell" ]
      else
        [ ] ++ [
          "v-at-fourfold"
          "v-a-singlestorey"
          "v-i-zshaped"
          "v-g-singlestorey"
          "v-l-zshaped"
          "v-brace-straight"
          "v-numbersign-slanted"
          "v-asterisk-hexlow"
        ];
      family = "Iosevka Baba";
    };

    set = "baba";
    extraParameters =
      if withLigature then builtins.readFile ./iosevka.toml else null;
    # TODO: extraParameters
  });
  iosevkaTermBaba = (unstable.pkgs.iosevka.override {
    privateBuildPlan = {
      design = [
        "term"
        "v-at-fourfold"
        "v-a-singlestorey"
        "v-i-zshaped"
        "v-g-singlestorey"
        "v-l-zshaped"
        "v-brace-straight"
        "v-numbersign-slanted"
        "v-asterisk-hexlow"
      ];
      family = "Iosevka Term Baba";
    };
    set = "term-baba";
  });
in {
  imports = [ ./firefox.nix ./i3.nix ./polybar.nix ./termite.nix ];

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "altgr-intl";
      displayManager.startx.enable = true;
      libinput = {
        enable = true;
        tapping = false;
        disableWhileTyping = true;
      };
    };

    sound.enable = true;
    hardware.pulseaudio.enable = true;

    environment.systemPackages = with pkgs; [ pamixer pavucontrol ];

    # TODO: generate with IosevkaGenHS
    fonts = {
      fonts = [
        iosevkaBaba
        iosevkaTermBaba
        pkgs.nur.repos.babariviere.nerd-font-symbols
      ];
      enableDefaultFonts = true;
    };
  };
}
