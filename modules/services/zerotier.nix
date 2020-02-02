{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.dotfiles.services.zerotier;

  mkHost = node: { "${node.ip}" = [ "${node.name}.zt" ]; };

  merge = list: foldl (a: b: a // b) { } list;
in {
  options.dotfiles.services.zerotier = {
    enable = mkEnableOption "zerotier";

    networks = mkOption {
      type = types.listOf types.str;
      description = "Networks to join";
      default = [ ];
    };

    nodes = mkOption {
      type = types.listOf (types.submodule {
        options = {
          name = mkOption {
            type = types.str;
            description = "node's name";
          };

          ip = mkOption {
            type = types.str;
            description = "node's ip";
          };
        };
      });
      description = "List of all nodes on network. Used to create hosts.";
      default = [ ];
    };
  };

  config = mkIf cfg.enable {
    services.zerotierone = {
      enable = true;
      joinNetworks = cfg.networks;
    };

    networking.hosts = merge (map mkHost cfg.nodes);
  };
}
