{ config }:

{
  assertions = [{
    assertion = config.environment.variables.XDG_CURRENT_DESKTOP != "";
    message =
      "environment.variables.XDG_CURRENT_DESKTOP must be set to make screensharing available";
  }];

  profiles.media.pipewire.enable = true;

  xdg.portal = {
    enable = true;
    wlr.enable = true;
  };

  environment.variables.XDG_SESSION_TYPE = "wayland";
}
