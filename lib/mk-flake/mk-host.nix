{ home, hostDefaults, inputs, lib, profiles, self }:

host:
let
  system = host.system or "x86_64-linux";
  platform = rec {
    inherit system;
    isDarwin = builtins.elem system lib.platforms.darwin;
    isLinux = builtins.elem system lib.platforms.linux;
    name = if isDarwin then "darwin" else "linux";
  };
  getDefaults = platform: attribute:
    let
      isList = builtins.elem attribute [ "modules" ];
      concat = if isList then
        (lib.fold (a: b: a ++ b) [ ])
      else
        (lib.fold (a: b: a // b) { });
      default = if isList then [ ] else { };
    in concat [
      (hostDefaults.common."${attribute}" or default)
      (hostDefaults.platform."${platform.name}"."${attribute}" or default)
      (hostDefaults.system."${platform.system}"."${attribute}" or default)
    ];

  hmConfig = {
    home-manager.sharedModules = (home.modules or [ ])
      ++ (home.profiles or [ ]);
    home-manager.extraSpecialArgs = { inherit inputs; };
  };

  mkSystem = system:
    let
      f = if platform.isDarwin then
        inputs.darwin.lib.darwinSystem
      else
        inputs.nixpkgs.lib.nixosSystem;
      getDefaults' = getDefaults platform;
    in f ({
      modules = (getDefaults' "modules") ++ [ hmConfig ] ++ profiles
        ++ host.modules;
      # extraArgs = (getDefaults "extraArgs") // host.extraArgs;
      specialArgs = {
        inherit self inputs platform system;
      } // (getDefaults' "specialArgs") // (host.specialArgs or { });
    } // (lib.optionalAttrs (platform.name == "linux") { inherit system; }));

in mkSystem system
