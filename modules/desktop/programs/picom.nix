{ config, lib, pkgs, ... }:

with lib;
with builtins;

let
  dotfiles = config.dotfiles;
  cfg = dotfiles.desktop.picom;

  pairOf = x: with types; addCheck (listOf x) (y: length y == 2);

  floatBetween = a: b:
    with lib;
    with types;
    addCheck str (x: versionAtLeast x a && versionOlder x b);

  toConf = attrs:
    concatStringsSep "\n" (mapAttrsToList (k: v:
      let
        sep = if isAttrs v then ":" else "=";
        # Basically a tinkered lib.generators.mkKeyValueDefault
        mkValueString = v:
          if isBool v then
            boolToString v
          else if isInt v then
            toString v
          else if isFloat v then
            toString v
          else if isString v then
            ''"${escape [ ''"'' ] v}"''
          else if isList v then
            "[ " + concatMapStringsSep " , " mkValueString v + " ]"
          else if isAttrs v then
            "{ " + concatStringsSep " " (mapAttrsToList
              (key: value: "${toString key}=${mkValueString value};") v) + " }"
          else
            abort "picom.mkValueString: unexpected type (v = ${v})";
      in "${escape [ sep ] k}${sep}${mkValueString v};") attrs);

  configFile = pkgs.writeText "picom.conf" (toConf cfg.settings);

  picom = pkgs.picom.overrideAttrs (old: { src = pkgs.sources.picom; });
in {
  options.dotfiles.desktop.picom = {
    enable = lib.mkEnableOption "picom";
    settings = let
      configTypes = with types; oneOf [ bool int float str ];
      # types.loaOf converts lists to sets
      loaOf = t: with types; either (listOf t) (attrsOf t);
    in mkOption {
      type = loaOf (types.either configTypes
        (loaOf (types.either configTypes (loaOf configTypes))));
      default = { };
      description = ''
        Additional Picom configuration.
      '';
    };
  };

  config = lib.mkIf (dotfiles.desktop.enable && cfg.enable) {
    dotfiles.desktop.picom.settings = {
      shadow = true;
      shadow-radius = 12;
      shadow-offset-x = -12;
      shadow-offset-y = -12;
      shadow-exclude = [
        "name = 'Notification'"
        "class_g = 'Conky'"
        "class_g ?= 'Notify-osd'"
        "class_g = 'Cairo-clock'"
        "class_g = 'slop'"
        "class_g = 'Firefox' && argb"
        "class_g = 'Rofi'"
        "_GTK_FRAME_EXTENTS@:c"
        "_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'"
      ];

      log-level = "ERROR";
      log-file = "/tmp/picom.log";

      blur = {
        method = "dual_kawase";
        strength = 6.9;
      };
      blur-background-frame = true;
      blur-background-fixed = true;
      blur-background-exclude = [
        "window_type = 'desktop'"
        "window_type = 'utility'"
        "window_type = 'notification'"
        "class_g = 'slop'"
        "class_g = 'Firefox' && argb"
        "name = 'rofi - Search'"
        "_GTK_FRAME_EXTENTS@:c"
      ];

      fading = true;
      fade-delta = 3;
      fade-in-step = 3.0e-2;
      fade-out-step = 3.0e-2;

      backend = "glx";
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      # use-ewmh-active-win = true;
      detect-rounded-corners = true;
      detect-client-opacity = true;
      refresh-rate = 0;
      vsync = true;
      # sw-opti = true;
      unredir-if-possible = false;
      # unredir-if-possible-delay = 5000;
      # unredir-if-possible-exclude = [ ];
      # focus-exclude = [ "class_g = 'Cairo-clock'" ];

      focus-exclude = [
        "class_g = 'Cairo-clock'"
        "class_g ?= 'rofi'"
        "class_g ?= 'slop'"
        "class_g ?= 'Steam'"
      ];

      detect-transient = true;
      detect-client-leader = true;
      invert-color-include = [ ];
      # resize-damage = 1;

      # GLX backend

      glx-no-stencil = true;
      # glx-no-rebind-pixmap = true;
      # xrender-sync-fence = true;
      use-damage = true;

      # Window type settings

      wintypes = {
        tooltip = {
          fade = true;
          shadow = false;
          focus = false;
        };
        normal = { shadow = false; };
        dock = { shadow = false; };
        dnd = { shadow = false; };
        popup_menu = {
          shadow = true;
          focus = false;
          opacity = 0.9;
        };
        dropdown_menu = {
          shadow = false;
          focus = false;
        };
        above = { shadow = true; };
        splash = { shadow = false; };
        utility = {
          focus = false;
          shadow = false;
        };
        notification = { shadow = false; };
        desktop = { shadow = false; };
        menu = { focus = false; };
        dialog = { shadow = true; };
      };
    };
    systemd.user.services.picom = {
      description = "Picom composite manager";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];

      # Temporarily fixes corrupt colours with Mesa 18
      environment =
        mkIf (cfg.settings.backend == "glx") { allow_rgb10_configs = "false"; };

      serviceConfig = {
        ExecStart = "${picom}/bin/picom --config ${configFile}";
        RestartSec = 3;
        Restart = "always";
      };
    };

    environment.systemPackages = [ picom ];
  };
}
