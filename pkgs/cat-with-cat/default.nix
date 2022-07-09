{ fetchFromGitHub, writeShellScriptBin }:

let
  src = fetchFromGitHub {
    owner = "GuidoFe";
    repo = "bash-cat-with-cat";
    rev = "b97e8bf9e997fa51915580555edb7d5aabec7777";
    sha256 = "sha256-fm4dFDBITlX38rdBqJQqpAsnk3ALHqoo7MklgdCWsho=";
  };
in writeShellScriptBin "cat-with-cat" (builtins.readFile "${src}/cat.sh")
