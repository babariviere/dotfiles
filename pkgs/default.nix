final: prev:

{
  cat-with-cat = prev.callPackage ./cat-with-cat { };
  emacsOsx = prev.callPackage ./emacs { };
  amber-emacs = prev.callPackage ./amber-emacs { emacs = final.emacsOsx; };
  lima = prev.callPackage ./lima { };
  tailscale = prev.callPackage ./tailscale { inherit (prev) tailscale; };
} // (prev.lib.optionalAttrs (prev.system == "x86_64-darwin") {
  lunchy = prev.callPackage ./lunchy { };
})
