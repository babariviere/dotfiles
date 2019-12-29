{ config, lib, pkgs, ... }:

with lib;
let cfg = config.dotfiles.services.zerotier;
in {
  options.dotfiles.services.zerotier = {
    networks = mkOption {
      type = types.listOf types.str;
      description = "Networks to join";
    };
  };

  config = mkIf cfg.enable {
    services.zerotierone = {
      enable = true;
      joinNetworks = cfg.networks;
    };
  };
}
