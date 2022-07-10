(import ./default.nix).shellFor {
  withHoogle = true;

  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
