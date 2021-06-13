{
  description = "Example darwin system flake";

  inputs = {
    nix.url = "github:nixos/nix";
    nix.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    neovim-nightly.url = "github:nix-community/neovim-nightly-overlay";
    flow.url = "path:../../../flow";

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
        home-manager.useGlobalPkgs = true;

        nix = {
          binaryCaches =
            [ "https://cache.nixos.org" "https://srid.cachix.org" ];
          binaryCachePublicKeys = [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            "srid.cachix.org-1:MTQ6ksbfz3LBMmjyPh0PLmos+1x+CdtJxA/J2W+PQxI="
          ];
          gc = {
            automatic = true;
            options = "-d --delete-older-than 7d";
          };
          package = pkgs.hiPrio pkgs.nixUnstable;
          registry = { nixpkgs.flake = nixpkgs; };
          trustedUsers = [ "babariviere" ];
          useSandbox = "relaxed";
        };

        nixpkgs.overlays = let
          nixOverlay = final: prev: {
            aws-sdk-cpp =
              prev.aws-sdk-cpp.overrideAttrs (attrs: { doCheck = false; });
          };
          javaOverlay = final: prev: {
            # FIXME: does not work
            graalvm11-ce = prev.graalvm11-ce.overrideAttrs (attrs: {
              doInstallCheck = false;
            });
          };
        in [ inputs.neovim-nightly.overlay inputs.emacs.overlay javaOverlay self.overlay ];

        services.nix-daemon.enable = true;
      };
    in {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake ./modules/examples#darwinConfigurations.mac-fewlines.system \
      #       --override-input darwin .
      darwinConfigurations."Baba-Mac" = darwin.lib.darwinSystem {
        modules = [
          configuration
          home-manager.darwinModules.home-manager
          ./hosts/mac-fewlines.nix
        ];
        specialArgs = { inherit inputs; };
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."Baba-Mac".pkgs;

      overlay = import ./pkgs;
    };
}
