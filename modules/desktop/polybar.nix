{ config, lib, pkgs, ... }:

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.polybar;
in {
  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    home-manager.users."${dotfiles.user}".services.polybar = {
      enable = true;
      package = pkgs.polybar.override {
        i3GapsSupport = config.dotfiles.desktop.i3.enable;
        alsaSupport = true;
        pulseSupport = true;
      };
      extraConfig = builtins.readFile (pkgs.mutate <config/polybar/config>
        dotfiles.colors); # TODO: use config field
      script = "polybar top &";
    };

    environment.systemPackages = with pkgs; [ acpi ];

    fonts.fonts = with pkgs; [ siji ];
  };
}
