{ config, pkgs }:

{
  assertions = [{
    assertion = config.environment.variables.XDG_CURRENT_DESKTOP != "";
    message =
      "environment.variables.XDG_CURRENT_DESKTOP must be set to make screensharing available";
  }];

  profiles.media.pipewire.enable = true;

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-wlr ];
    # FIXME: broken font
    # gtkUsePortal = true;
  };

  environment.variables.XDG_SESSION_TYPE = "wayland";
}
