{ lib }:

{ inputs, self, ... }@config:

let
  hosts = config.hosts or { };

  hostDefaults = config.hostDefaults or { };

  profiles = config.profiles or [ ];

  home = config.home or { };

  mkHost = import ./mk-host.nix {
    inherit inputs lib hostDefaults self profiles home;
  };

  mkHome = import ./mk-home.nix {
    inherit inputs home;
  };

  isLinux = _n: v: builtins.elem v.system lib.platforms.linux;
  isDarwin = _n: v: builtins.elem v.system lib.platforms.darwin;
  isHome = _n: v: v.system == "home";
in {
  nixosConfigurations = lib.mapAttrs (_: mkHost)
    (lib.filterAttrs isLinux hosts);

  darwinConfigurations = lib.mapAttrs (_: mkHost)
    (lib.filterAttrs isDarwin hosts);

  homeConfigurations = lib.mapAttrs (_: mkHome)
    (lib.filterAttrs isHome hosts);
} // (lib.filterAttrs (n: _:
  !(builtins.elem n [
    "self"
    "inputs"
    "hosts"
    "hostDefaults"
    "profiles"
    "home"
  ])) config)
