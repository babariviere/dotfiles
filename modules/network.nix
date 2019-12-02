{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.dotfiles.network;
  ethCfg = mkIf (cfg.eth != "") { "${cfg.eth}".useDHCP = true; };
  wlanCfg = mkIf (cfg.wlan != "") { "${cfg.wlan}".useDHCP = true; };
in {
  options.dotfiles.network = {
    eth = mkOption {
      type = types.str;
      description = "Network interface for ethernet";
    };
    wlan = mkOption {
      type = types.str;
      description = "Network interface for wifi";
    };
  };

  config = {
    networking.useDHCP = cfg.eth == "" && cfg.wlan == "";
    networking.interfaces = mkMerge [ ethCfg wlanCfg ];
  };
}
