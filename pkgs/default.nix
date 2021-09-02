final: prev:

{
  cat-with-cat = prev.callPackage ./cat-with-cat { };
  emacsGit = prev.callPackage ./emacs { emacsGit = prev.emacsGit; };
  emacs-client = prev.callPackage ./emacs-client { emacs = final.emacsGit; };
  amber-emacs = prev.callPackage ./amber-emacs { emacs = final.emacsGit; };
  lima = prev.callPackage ./lima { };
  tailscale = prev.callPackage ./tailscale { inherit (prev) tailscale; };
} // (prev.lib.optionalAttrs (prev.system == "x86_64-darwin") {
  lunchy = prev.callPackage ./lunchy { };
})
