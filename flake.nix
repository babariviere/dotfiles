{
  description = "Baba's NixOS system.";

  inputs.nixpkgs.url = "nixpkgs/release-20.03";
  inputs.unstable.url = "github:NixOS/nixpkgs/master";
  inputs.home.url = "github:babariviere/home-manager/flakes";

  inputs.arion = {
    url = "github:hercules-ci/arion/master";
    flake = false;
  };
  inputs.snack = {
    url = "github:nmattia/snack/master";
    flake = false;
  };
  inputs.emacs.url = "github:nix-community/emacs-overlay";
  inputs.nur.url = "github:babariviere/NUR/flakes";
  inputs.nur.inputs.nixpkgs.follows = "nixpkgs";

  inputs.plymouth-themes = {
    url = "github:adi1090x/plymouth-themes";
    flake = false;
  };

  inputs.zgen = {
    url = "github:tarjoilija/zgen";
    flake = false;
  };

  outputs = inputs@{ self, home, nixpkgs, unstable, ... }:
    let
      system = "x86_64-linux";

      pkgs = import nixpkgs {
        inherit system;
        overlays = self.overlays;
        config.allowUnfree = true;
      };
    in {
      nixosConfigurations = import ./hosts (inputs // { inherit system pkgs; });

      overlay = import ./pkgs;

      overlays = let
        unstable = import unstable {
          inherit system;
          config.allowUnfree = true;
        };
        overlays = map (name: import (./overlays + "/${name}"))
          (builtins.attrNames (builtins.readDir ./overlays));
        unstableOverlay = final: prev: { inherit unstable; };
        # TODO: how to expose overlays
      in [
        (final: prev: { inherit inputs; })
        unstableOverlay
        inputs.emacs.overlay
        inputs.nur.overlay
        (final: prev: {
          arion = (import final.inputs.arion { inherit (final) pkgs; }).arion;
        })
        (final: prev: { snack = (import final.inputs.snack).snack-exe; })
        (final: prev: {
          podman = unstable.podman;
          podman-unwrapped = unstable.podman-unwrapped;
        })
      ] ++ overlays;

      nixosModules = { };

      # TODO: nixos modules
      # nixosModules = let
      #   prep = map (path: {
      #     name = removeSuffix ".nix" (baseNameOf path);
      #     value = import path;
      #   });

      #   # modules
      #   moduleList = import ./modules/list.nix;
      #   modulesAttrs = listToAttrs (prep moduleList);

      #   # profiles
      #   profilesList = import ./profiles/list.nix;
      #   profilesAttrs = { profiles = listToAttrs (prep profilesList); };

      # in modulesAttrs // profilesAttrs;

    };
}
