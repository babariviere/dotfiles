{ stdenvNoCC }:

file: args:
(stdenvNoCC.mkDerivation (args // {
  name = baseNameOf file;
  phases = [ "installPhase" ];
  preferLocalBuild = true;
  allowSubstitutes = false;

  installPhase = ''
    cp ${file} $out
    substituteAllInPlace $out
  '';
}))
