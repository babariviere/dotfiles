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
        overlays = builtins.attrValues self.overlays;
        config.allowUnfree = true;
      };
    in {
      nixosConfigurations = import ./hosts (inputs // { inherit system pkgs; });

      overlay = import ./pkgs;

      overlays = let
        removeSuffix = suffix: str: builtins.head (builtins.split suffix str);

        uns = import unstable {
          inherit system;
          config.allowUnfree = true;
        };
        overlays = builtins.foldl' (prev: name:
          prev // {
            ${removeSuffix ".nix" name} = import (./overlays + "/${name}");
          }) { } (builtins.attrNames (builtins.readDir ./overlays));
        # TODO: how to expose overlays
      in {
        unstable = final: prev: { unstable = uns; };
        emacs = inputs.emacs.overlay;
        nur = inputs.nur.overlay;
        arion = final: prev: {
          arion = (import final.inputs.arion { inherit (final) pkgs; }).arion;
        };
        snack = final: prev: { snack = (import final.inputs.snack).snack-exe; };
        podman = final: prev: {
          podman = uns.podman;
          podman-unwrapped = uns.podman-unwrapped;
        };
        plymouth = final: prev: {
          plymouth-themes = final.callPackage ./pkgs/plymouth-themes {
            src = inputs.plymouth-themes;
          };
        };
      } // overlays;

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
