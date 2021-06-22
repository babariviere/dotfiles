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
    flow.url = "path:../flow";

    # Emacs
    emacs.url = "github:nix-community/emacs-overlay";
    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };
    # FIXME: does not work for now
    # nix-doom-emacs.url = "github:vlaci/nix-doom-emacs";
    # nix-doom-emacs.inputs.doom-emacs.follows = "doom-emacs";
  };

  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs:
    let
      configuration = { config, pkgs, ... }: {
        home-manager.useUserPackages = true;

        nix = {
          binaryCaches =
            [ "https://cache.nixos.org" "https://nix-community.cachix.org" ];
          binaryCachePublicKeys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          ];
          gc = {
            automatic = true;
            options = "-d --delete-older-than 7d";
          };
          package = pkgs.hiPrio pkgs.nixUnstable;
          registry = { nixpkgs.flake = nixpkgs; };
          # trustedUsers = [ "babariviere" ];
          useSandbox = "relaxed";
        };

        nixpkgs.overlays = let
          nixOverlay = final: prev: {
            aws-sdk-cpp =
              prev.aws-sdk-cpp.overrideAttrs (attrs: { doCheck = false; });
          };
          javaOverlay = final: prev: {
            # FIXME: does not work
            graalvm11-ce = prev.graalvm11-ce.overrideAttrs
              (attrs: { doInstallCheck = false; });
          };
        in [
          inputs.neovim-nightly.overlay
          inputs.emacs.overlay
          javaOverlay
          self.overlay
        ];

        services.nix-daemon.enable = true;
      };

      lib = nixpkgs.lib.extend
        (self: super: { my = import ./lib { lib = self; }; });

      modules = lib.my.findModulesRec ./modules;
    in {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake ./modules/examples#darwinConfigurations.mac-fewlines.system \
      #       --override-input darwin .
      darwinConfigurations."Baba-Mac" = darwin.lib.darwinSystem {
        modules = [ configuration home-manager.darwinModules.home-manager ]
          ++ modules ++ [ ./hosts/mac-fewlines.nix ];
        specialArgs = { inherit inputs lib; };
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."Baba-Mac".pkgs;

      lib = lib.my;

      overlay = import ./pkgs;

      packages.x86_64-darwin = {
        lima = nixpkgs.legacyPackages.x86_64-darwin.callPackage ./pkgs/lima { };
      };
    };
}
