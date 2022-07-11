{
  description = "Baba's dotfiles";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:nixos/nixos-hardware";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    utils.url = "github:numtide/flake-utils";
    deploy.url = "github:serokell/deploy-rs";

    # Emacs
    emacs.url = "github:nix-community/emacs-overlay";
    emacs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, darwin, nixpkgs, home-manager, utils, deploy, ... }@inputs:
    let
      inherit (nixpkgs) lib;

      lib' = import ./lib { inherit lib; };
      modules = import ./system/modules/module-list.nix;

    in lib'.mkFlake {
      inherit self inputs;

      profiles = lib'.foldProfiles ./system/profiles;

      hostDefaults = {
        common = {
          modules = [ (import ./system/defaults) ] ++ modules;
          specialArgs = { network = import ./network.nix; };
        };
        platform = {
          darwin.modules =
            [ (import ./system/defaults/darwin.nix) home-manager.darwinModules.home-manager ];
          linux.modules = [
            (import ./system/defaults/linux.nix)
            home-manager.nixosModules.home-manager
          ];
        };
      };

      hosts = {
        loki = {
          system = "x86_64-linux";
          modules = [ ./hosts/loki/configuration.nix ];
        };

        buri = {
          system = "x86_64-linux";
          modules = [ ./hosts/buri/configuration.nix ];
        };

	      nanna = {
	        system = "home";
	        modules = [ ./hosts/nanna/home.nix ];
	      };
      };

      home = {
        modules = import ./home/modules/module-list.nix;
        profiles = lib'.foldProfiles ./home/profiles;
      };

      # Expose the package set, including overlays, for convenience.
      # darwinPackages = self.darwinConfigurations."ochatt".pkgs;

      deploy.nodes = {};

      checks = builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy) deploy.lib;

      overlay = import ./pkgs;

      overlays = {
        emacs = inputs.emacs.overlay;
        self = self.overlay;
      };
    } // (utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = lib.attrValues self.overlays;
        };
        self' = self;
      in {
        packages = (lib.fix (self: self'.overlay self pkgs)) // {
          inherit (pkgs) emacsGit;
        };

        devShell = pkgs.mkShell {
          buildInputs = [
            deploy.defaultPackage."${system}"
          ];
        };

        apps = {
          amber-emacs = {
            type = "app";
            program = "${self.packages."${system}".amber-emacs}/bin/emacs";
          };
        };
      }));
}
