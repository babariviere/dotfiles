final: prev:

{
  emacsOsx = prev.callPackage ./emacs { };
  lima = prev.callPackage ./lima { };
}
