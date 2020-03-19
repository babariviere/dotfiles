self: super:

{
  mutate = self.callPackage ./mutate { };

  iosevkaBaba = (self.callPackage ./iosevka { }) "Iosevka Baba" "baba" true;
  iosevkaTermBaba =
    (self.callPackage ./iosevka { }) "Iosevka Term Baba" "term-baba" false;

  nix-patch = self.callPackage ./nix-patch { };

  nixos-update = self.callPackage ./nixos-update { };

  prime = self.callPackage ./prime { };

  xterm-24bits = self.callPackage ./xterm-24bits { };
}
