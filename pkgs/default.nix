final: prev:

{
  emacsOsx = prev.callPackage ./emacs { };
  lima = prev.callPackage ./lima { };
  tailscale = prev.callPackage ./tailscale { inherit (prev) tailscale; };
}
