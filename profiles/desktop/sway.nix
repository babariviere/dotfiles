{ pkgs }:

{
  programs.sway.enable = true;
  environment.variables.XDG_CURRENT_DESKTOP = "sway";

  profiles.login.greetd.command = "${pkgs.sway}/bin/sway";
  profiles.desktop.wayland.enable = true;

  programs.dconf.enable = true;
}
