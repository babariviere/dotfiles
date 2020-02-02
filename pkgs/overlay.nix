self: super:

{
  mutate = self.callPackage ./mutate { };

  iosevkaBaba = (self.callPackage ./iosevka { }) "Iosevka Baba" "baba" true;
  iosevkaTermBaba =
    (self.callPackage ./iosevka { }) "Iosevka Term Baba" "term-baba" false;

  xterm-24bits = self.callPackage ./xterm-24bits { };
}
