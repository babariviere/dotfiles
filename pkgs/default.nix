final: prev:

{
  emacsOsx = prev.callPackage ./emacs { };
  lima = prev.callPackage ./lima { };
  lunchy = prev.callPackage ./lunchy { };
  tailscale = prev.callPackage ./tailscale { inherit (prev) tailscale; };
}
