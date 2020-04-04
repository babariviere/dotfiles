let
  sources = import ./nix/sources.nix;
  overlay = self: super: {
    inherit sources;

    arion = (import sources.arion { inherit (self) pkgs; }).arion;
    snack = (import sources.snack).snack-exe;
    nur = import sources.NUR { inherit (self) pkgs; };
    unstable = import sources.unstable { config = { allowUnfree = true; }; };
  };
in [ (import ./pkgs/overlay.nix) overlay (import sources.emacs-overlay) ]
