final: prev:

{
  cat-with-cat = prev.callPackage ./cat-with-cat { };
  amber-emacs = prev.callPackage ./amber-emacs { emacs = prev.emacsGitNativeComp; };
  lima = prev.callPackage ./lima { };
  # FIXME: unable to patch
  # nixUnstable = prev.callPackage ./nix { nix = prev.nixUnstable; };
  tailscale = prev.callPackage ./tailscale { inherit (prev) tailscale; };
  gitlab-ci-lint = prev.callPackage ./gitlab-ci-lint { };
  biosevka = prev.callPackage ./biosevka { };
  lxd = prev.callPackage ./lxd { useQemu = true; };
}
# // (prev.lib.optionalAttrs (prev.system == "x86_64-darwin") {
#   emacs-client = prev.callPackage ./emacs-client { emacs = prev.emacsGit; };
#   lunchy = prev.callPackage ./lunchy { };
# })
