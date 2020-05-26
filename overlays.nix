let
  sources = import ./nix/sources.nix;

  unstable = import sources.unstable { config = { allowUnfree = true; }; };
  overlay = self: super: {
    inherit sources;

    arion = (import sources.arion { inherit (self) pkgs; }).arion;
    snack = (import sources.snack).snack-exe;
    nur = import sources.NUR { inherit (self) pkgs; };

    unstable = import <nixpkgs-unstable> { config = { allowUnfree = true; }; };
    podman = unstable.callPackage
      "${sources.unstable}/pkgs/applications/virtualization/podman/wrapper.nix"
      { };
    podman-unwrapped = unstable.callPackage
      "${sources.unstable}/pkgs/applications/virtualization/podman" { };
  };
in [ (import ./pkgs/overlay.nix) overlay (import sources.emacs-overlay) ]
