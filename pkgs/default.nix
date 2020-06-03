final: prev:

{
  ert-run = final.callPackage ./ert-run { };

  guix = final.callPackage ./guix { };

  iosevkaBaba = (final.callPackage ./iosevka { }) "Iosevka Baba" "baba" true;
  iosevkaTermBaba =
    (final.callPackage ./iosevka { }) "Iosevka Term Baba" "term-baba" false;

  mutate = final.callPackage ./mutate { };

  nodePackages = prev.nodePackages // (final.callPackages ./node-packages { });

  nix-patch = final.callPackage ./nix-patch { };

  prime = final.callPackage ./prime { };

  xterm-24bits = final.callPackage ./xterm-24bits { };
}
