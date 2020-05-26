inputs@{ home, nixpkgs, unstable, self, pkgs, system, ... }:

let
  inherit (nixpkgs) lib;

  utils = import ../lib/utils.nix { inherit lib; };

  inherit (utils) recImport;

  inherit (builtins) attrValues removeAttrs;

  config = hostName:
    lib.nixosSystem {
      inherit system;

      specialArgs.unsmod = name: "${unstable}/${name}";
      specialArgs.priv = name: ../. + "/private/${hostName}/${name}";
      specialArgs.usr = { inherit utils; };
      specialArgs.usrconf = name: ../. + "/config/${name}";
      specialArgs.inputs = inputs;

      modules = let
        inherit (home.nixosModules) home-manager;

        core = ../profiles/core.nix;

        global = {
          networking.hostName = hostName;
          nix.nixPath = [
            "nixpkgs=${nixpkgs}"
            "nixpkgs-unstable=${unstable}"
            "nixos-config=/etc/nixos/configuration.nix"
            "nixpkgs-overlays=/etc/nixos/overlays"
            "config=/etc/dotfiles/config"
          ];

          nixpkgs = { inherit pkgs; };

          home-manager.useUserPackages = true;
        };

        local = import "${toString ./.}/${hostName}.nix";

        # Everything in `./modules/list.nix`.
        flakeModules =
          attrValues (removeAttrs self.nixosModules [ "profiles" ]);

      in flakeModules ++ [ core global local home-manager ];
    };
in recImport {
  dir = ./.;
  _import = config;
}
