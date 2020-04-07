{ stdenv, src }:

stdenv.mkDerivation {
  name = "plymouth-themes";
  version = src.rev;

  inherit src;

  phases = [ "installPhase" "fixupPhase" ];

  installPhase = ''
    mkdir -p $out/share/plymouth/themes
    for f in $src/pack_1/*/ $src/pack_2/*/ $src/pack_3/*/ $src/pack_4/*/; do
      theme=$(basename $f)
      if [ $theme == "glow" ]; then
        continue
      fi
      cp -r $f $out/share/plymouth/themes/
    done
  '';

  fixupPhase = ''
    for f in $out/share/plymouth/themes/*/*.plymouth; do
      substituteInPlace $f --replace "/usr" "$out"
    done
  '';
}
