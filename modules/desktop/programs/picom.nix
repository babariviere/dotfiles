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
    dotfiles.desktop.picom.settings = import ./picom.conf.nix;
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
