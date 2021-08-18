{
  description = "Baba's dotfiles";

  inputs = {
    nix.url = "github:nixos/nix";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/master";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    flow.url = "github:babariviere/flow";
    utils.url = "github:numtide/flake-utils";
    deploy.url = "github:serokell/deploy-rs";
    agenix.url = "github:ryantm/agenix";
    hosts-denylist = {
      url = "github:StevenBlack/hosts";
      flake = false;
    };

    # Emacs
    emacs.url = "github:nix-community/emacs-overlay";
    emacs.inputs.nixpkgs.follows = "nixpkgs";
    # doom-emacs = {
    #   url = "github:hlissner/doom-emacs/develop";
    #   flake = false;
    # };
    # FIXME: does not work for now
    # nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    # nix-doom-emacs.inputs.doom-emacs.follows = "doom-emacs";
  };

  outputs = { self, darwin, nixpkgs, home-manager, utils, deploy, ... }@inputs:
    let
      configuration = { config, pkgs, ... }: {
        home-manager.useUserPackages = true;
        home-manager.useGlobalPkgs = true;

        nix = {
          binaryCaches = [
            "http://nix-store.vercar.home"
            "https://cache.nixos.org"
            "https://cache.ngi0.nixos.org/"
            "https://nix-community.cachix.org"
            "https://babariviere.cachix.org"
          ];
          binaryCachePublicKeys = [
            "vercar.home:1R/Q2IoFGQ5NqYGneOoxhrvkhgpdBrn87Po2uPM4wmI="
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "cache.ngi0.nixos.org-1:KqH5CBLNSyX184S9BKZJo1LxrxJ9ltnY2uAs5c/f1MA="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            "babariviere.cachix.org-1:igoOZJyEAhWg3Rbavjim3yyDj7nIkGYe5327+G0diFw="
          ];
          extraOptions = ''
            experimental-features = nix-command flakes
            keep-outputs = true
            keep-derivations = true
          '';
          gc = {
            automatic =
              false; # FIXME: enable only on local machines, on server it's useful
            options = "-d --delete-older-than 7d";
          };
          package = pkgs.hiPrio pkgs.nixUnstable;
          registry = {
            nixpkgs.flake = nixpkgs;
            dotfiles.flake = self;
          };
          useSandbox = "relaxed";
        };

        nixpkgs = {
          overlays = lib.attrValues self.overlays;

          # FIXME: https://github.com/NixOS/nix/issues/4903
          # config.contentAddressedByDefault = true;
        };
      };

      darwinConfiguration = { config, pkgs, ... }: {
        nix.sandboxPaths = [
          "/System/Library/Frameworks"
          "/System/Library/PrivateFrameworks"
          "/usr/lib"
          "/private/tmp"
          "/private/var/tmp"
          "/usr/bin/env"
        ];
        services.nix-daemon = {
          enable = true;
          logFile = "/var/log/nix-daemon.log";
        };
      };

      inherit (nixpkgs) lib;

      # lib = nixpkgs.lib.extend
      #   (self: super: { my = import ./lib { lib = self; }; });

      lib' = import ./lib { inherit lib; };
      modules = import ./modules/module-list.nix;

    in lib'.mkFlake {
      inherit self inputs;

      profiles = lib'.foldProfiles ./profiles;

      hostDefaults = {
        common.modules = [ configuration ] ++ modules;
        platform = {
          darwin.modules =
            [ darwinConfiguration home-manager.darwinModules.home-manager ];
          linux.modules = [
            home-manager.nixosModules.home-manager
            inputs.agenix.nixosModules.age
          ];
        };
      };

      hosts = {
        ochatt = {
          system = "x86_64-darwin";
          modules = [ ./hosts/ochatt.nix ];
          specialArgs = { inherit inputs lib; };
        };

        vercar = {
          system = "x86_64-linux";
          modules = [ ./hosts/vercar/configuration.nix ];
        };
      };

      home = {
        modules = import ./home/modules/module-list.nix;
        profiles = lib'.foldProfiles ./home/profiles;
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."ochatt".pkgs;

      deploy.nodes = {
        vercar = {
          hostname = "100.100.28.13";
          profiles.system = {
            user = "root";
            sshUser = "root";
            path = deploy.lib.x86_64-linux.activate.nixos
              self.nixosConfigurations.vercar;
          };
        };
      };

      checks = builtins.mapAttrs
        (system: deployLib: deployLib.deployChecks self.deploy) deploy.lib;
      # lib = lib.my;

      overlay = import ./pkgs;

      overlays = let
        pythonOverride = super: python-self: python-super: {
          # https://github.com/NixOS/nixpkgs/issues/128266
          pycairo = python-super.pycairo.overrideAttrs (attrs: {
            doCheck = false;
            doInstallCheck = false;
            nativeBuildInputs =
              builtins.filter (drv: drv.name != "pytest-check-hook")
              attrs.nativeBuildInputs;
          });
        };
        pythonOverlay = final: prev: {
          python =
            prev.python.override { packageOverrides = pythonOverride prev; };
          python3 =
            prev.python3.override { packageOverrides = pythonOverride prev; };
          python38 =
            prev.python38.override { packageOverrides = pythonOverride prev; };
        };
      in {
        neovim = inputs.neovim-nightly.overlay;
        emacs = inputs.emacs.overlay;
        python = pythonOverlay;
        self = self.overlay;
      };
    } // (utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = lib.attrValues self.overlays;
        };
        self' = self;
      in {
        packages = lib.fix (self: self'.overlay self pkgs);

        devShell = pkgs.mkShell {
          buildInputs = [
            inputs.agenix.defaultPackage."${system}"
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
