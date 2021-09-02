final: prev:

{
  cat-with-cat = prev.callPackage ./cat-with-cat { };
  emacsGit = prev.callPackage ./emacs { emacsGit = prev.emacsGit; };
  emacs-client = prev.runCommand "emacs-client" { } ''
    mkdir -p $out/Applications
    cp -r ${../resources/EmacsClient.app} $out/Applications/EmacsClient.app
  '';
  amber-emacs = prev.callPackage ./amber-emacs { emacs = final.emacsGit; };
  lima = prev.callPackage ./lima { };
  tailscale = prev.callPackage ./tailscale { inherit (prev) tailscale; };
} // (prev.lib.optionalAttrs (prev.system == "x86_64-darwin") {
  lunchy = prev.callPackage ./lunchy { };
})
