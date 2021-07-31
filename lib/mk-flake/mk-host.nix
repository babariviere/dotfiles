{ home, hostDefaults, inputs, lib, profiles, self }:

host:
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

  hmConfig = {
    home-manager.sharedModules = (home.modules or [ ])
      ++ (home.profiles or [ ]);
  };

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
      modules = (getDefaults' "modules") ++ [ hmConfig ] ++ profiles
        ++ host.modules;
      # extraArgs = (getDefaults "extraArgs") // host.extraArgs;
      specialArgs = {
        inherit self inputs system;
      } // (getDefaults' "specialArgs") // host.specialArgs;
    } // (lib.optionalAttrs (platform == "linux") { inherit system; }));

in mkSystem system
