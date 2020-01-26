self: super:

{
  mutate = self.callPackage ./mutate { };

  xterm-24bits = self.callPackage ./xterm-24bits { };
}
