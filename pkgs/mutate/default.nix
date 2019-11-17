{ stdenvNoCC }:

file: args:
(stdenvNoCC.mkDerivation (args // {
  name = baseNameOf file;
  phases = [ "installPhase" ];

  installPhase = ''
    cp ${file} $out
    substituteAllInPlace $out
  '';
}))
