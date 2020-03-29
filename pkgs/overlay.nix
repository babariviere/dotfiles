self: super:

{
  ert-run = self.callPackage ./ert-run { };

  mutate = self.callPackage ./mutate { };

  iosevkaBaba = (self.callPackage ./iosevka { }) "Iosevka Baba" "baba" true;
  iosevkaTermBaba =
    (self.callPackage ./iosevka { }) "Iosevka Term Baba" "term-baba" false;

  nix-patch = self.callPackage ./nix-patch { };

  nixos-update = self.callPackage ./nixos-update { };

  prime = self.callPackage ./prime { };

  plymouth-themes =
    self.callPackage ./plymouth-themes { src = self.sources.plymouth-themes; };

  xterm-24bits = self.callPackage ./xterm-24bits { };
}
