{ src, stdenv, gtk-engine-murrine }:

stdenv.mkDerivation {
  name = "juno";

  inherit src;

  propagatedUserEnvPkgs = [ gtk-engine-murrine ];

  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/share/themes/Juno
    cp -a $src/* $out/share/themes/Juno
  '';
}
