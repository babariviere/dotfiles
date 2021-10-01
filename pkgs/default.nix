final: prev:

{
  cat-with-cat = prev.callPackage ./cat-with-cat { };
  amber-emacs = prev.callPackage ./amber-emacs { emacs = final.emacsGit; };
  lima = prev.callPackage ./lima { };
  tailscale = prev.callPackage ./tailscale { inherit (prev) tailscale; };
  gitlab-ci-lint = prev.callPackage ./gitlab-ci-lint { };
} // (prev.lib.optionalAttrs (prev.system == "x86_64-darwin") {
  emacsGit = prev.callPackage ./emacs { emacsGit = prev.emacsGit; };
  emacs-client = prev.callPackage ./emacs-client { emacs = final.emacsGit; };
  lunchy = prev.callPackage ./lunchy { };
})
