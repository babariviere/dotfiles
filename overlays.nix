let
  sources = import ./nix/sources.nix;

  fprintd-nixpkgs = import sources.fprintd-nixpkgs { };
  overlay = self: super: {
    inherit sources;

    arion = (import sources.arion { inherit (self) pkgs; }).arion;
    snack = (import sources.snack).snack-exe;
    nur = import sources.NUR { inherit (self) pkgs; };
    unstable = import sources.unstable { config = { allowUnfree = true; }; };

    fprintd_1_90 = fprintd-nixpkgs.fprintd;
    libfprint_1_90 = fprintd-nixpkgs.libfprint;
  };
in [ (import ./pkgs/overlay.nix) overlay (import sources.emacs-overlay) ]
