{ lib }:

{ inputs, self, ... }@config:

let
  hosts = config.hosts or { };

  hostDefaults = config.hostDefaults or { };

  mkHost = import ./mk-host.nix { inherit inputs lib hostDefaults self; };
in {
  nixosConfigurations = lib.mapAttrs (_: mkHost)
    (lib.filterAttrs (n: v: builtins.elem v.system lib.platforms.linux) hosts);

  darwinConfigurations = lib.mapAttrs (_: mkHost)
    (lib.filterAttrs (n: v: builtins.elem v.system lib.platforms.darwin) hosts);
} // (lib.filterAttrs
  (n: _: !(builtins.elem n [ "self" "inputs" "hosts" "hostDefaults" ])) config)
