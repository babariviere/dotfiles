{ lib }:

let
  read = path:
    if builtins.pathExists path then
      lib.removeSuffix "\n" (builtins.readFile path)
    else
      [ ];
in { GITHUB_TOKEN = read ./env/github_token; }
