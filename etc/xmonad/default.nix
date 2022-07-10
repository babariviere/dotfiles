let
  # Read in the Niv sources
  sources = import ./nix/sources.nix { };

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "amethyst";
    src = ./.;
  };
  modules = [{
    # Replace `extra-libraries` dependencies
    packages.X11.components.library.libs = pkgs.lib.mkForce
      (with pkgs.xorg; [ libX11 libXrandr libXext libXScrnSaver libXinerama ]);
  }];
  # Specify the GHC version to use.
  compiler-nix-name = "ghc902"; # Not required for `stack.yaml` based projects.
}
