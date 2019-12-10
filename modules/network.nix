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
    networking = {
      useDHCP = cfg.eth == "" && cfg.wlan == "";
      interfaces = mkMerge [ ethCfg wlanCfg ];
      wireless = mkIf (cfg.wlan != "") {
        enable = true;
        networks = import ../private/networks.nix;
        extraConfig = ''
          ctrl_interface=/run/wpa_supplicant
          ctrl_interface_group=wheel
        '';
      };

      nameservers = [ "8.8.8.8" "8.8.4.4" ];
    };
  };
}
