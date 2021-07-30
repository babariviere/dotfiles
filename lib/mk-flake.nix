{ lib }:

{ inputs, self, ... }@config:

let
  hosts = config.hosts or { };

  hostDefaults = config.hostDefaults or { };

  mapHost = host:
    let
      system = host.system or "x86_64-linux";
      getDefaults = { system, platform }:
        attribute:
        let
          isList = builtins.elem attribute [ "modules" ];
          concat = if isList then
            (lib.fold (a: b: a ++ b) [ ])
          else
            (lib.fold (a: b: a // b) { });
          default = if isList then [ ] else { };
        in concat [
          (hostDefaults.common."${attribute}" or default)
          (hostDefaults.platform."${platform}"."${attribute}" or default)
          (hostDefaults.system."${system}"."${attribute}" or default)
        ];

      mkSystem = system:
        let
          platform = if (builtins.elem system lib.platforms.darwin) then
            "darwin"
          else
            "linux";
          f = if platform == "darwin" then
            inputs.darwin.lib.darwinSystem
          else
            inputs.nixpkgs.lib.nixosSystem;
          getDefaults' = getDefaults { inherit system platform; };
        in f ({
          modules = (getDefaults' "modules") ++ host.modules;
          # extraArgs = (getDefaults "extraArgs") // host.extraArgs;
          specialArgs = {
            inherit self inputs system;
          } // (getDefaults' "specialArgs") // host.specialArgs;
        } // (lib.optionalAttrs (platform == "linux") { inherit system; }));

    in mkSystem system;
in {
  nixosConfigurations = lib.mapAttrs (_: mapHost)
    (lib.filterAttrs (n: v: builtins.elem v.system lib.platforms.linux) hosts);

  darwinConfigurations = lib.mapAttrs (_: mapHost)
    (lib.filterAttrs (n: v: builtins.elem v.system lib.platforms.darwin) hosts);
} // (lib.filterAttrs
  (n: _: !(builtins.elem n [ "self" "inputs" "hosts" "hostDefaults" ])) config)
