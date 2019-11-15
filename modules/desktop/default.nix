{ config, lib, pkgs, ... }:

with lib;
let
  unstable = import <nixpkgs-unstable> { };
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
  imports = [
    ./compton.nix
    ./firefox.nix
    ./i3.nix
    ./polybar.nix
    ./riot.nix
    ./termite.nix
  ];

  options.dotfiles.desktop = {
    enable = mkEnableOption "desktop";

    ligature = mkOption {
      type = types.bool;
      description = "Add ligature support to all services";
      default = true;
    };

    # TODO: merge
    # strategy is to set priorities (enum with value like first, middle, last with priority number)
    xinitrc = mkOption {
      type = types.str // {
        merge = loc: defs:
          lib.strings.concatMapStringsSep "\n" (x: x.value) defs;
      };
      description = ".xinitrc script to install";
      default = "";
    };
  };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      layout = "us";
      xkbVariant = "altgr-intl";
      displayManager.startx.enable = true;
      desktopManager.xterm.enable = false;
      libinput = {
        enable = true;
        tapping = false;
        disableWhileTyping = true;
      };
    };

    dotfiles.desktop.xinitrc = "test";

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
    home-manager.users."${config.dotfiles.user}".home = {
      # TODO: this is just for test, but this will actually replace i3 .xinitrc
      file = { ".xinitrc.test".text = config.dotfiles.desktop.xinitrc; };
    };
  };
}
